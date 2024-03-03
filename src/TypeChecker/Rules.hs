{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}

module TypeChecker.Rules
( inferTypeOf

)
where

import Types
import TypeChecker.TypingContext
import Control.Monad.Except ( throwError )
import Control.Monad ( liftM2 , guard , when )
import Data.List ( find )
import Prelude hiding ( id )

-- toTypeBool :: Bool -> TypeBool
-- toTypeBool True = TypeTrue
-- toTypeBool False = TypeFalse

inferTypeOf :: Expr -> EnvironmentP ID
inferTypeOf (XEqual e1 e2) = inferTypeOfBinop e1 e2 eqType
inferTypeOf (XGt e1 e2) = inferTypeOfBinop e1 e2 gtType
inferTypeOf (XLt e1 e2) = inferTypeOfBinop e1 e2 ltType
inferTypeOf (XAnd e1 e2) = inferTypeOfBinop e1 e2 andType
inferTypeOf (XOr e1 e2) = inferTypeOfBinop e1 e2 orType
inferTypeOf (XNot e) = call notType . (: []) =<< inferTypeOf e
inferTypeOf (XAdd e1 e2) = inferTypeOfBinop e1 e2 addType
inferTypeOf (XSub e1 e2) = inferTypeOfBinop e1 e2 subtractType
inferTypeOf (XInt i) = tryUpdateDAG $ TInt $ INumber i
inferTypeOf (XBool b) = tryUpdateDAG $ TBool $ BValue b
inferTypeOf (XArray es) = do
  id_ts <- inferTypeOfMany es
  id_lub <- union id_ts
  extra <- locateSpecificTypes id_lub id_ts 0
  tryUpdateDAG $ TArray (length es) id_lub extra
  where
    locateSpecificTypes glb (t : ts) curr = do
      res <- t `strictlyGt` glb
      ret <- locateSpecificTypes glb ts (curr + 1)
      if res then return $ (curr, t) : ret
      else return ret
    locateSpecificTypes _ [] _ = return []
inferTypeOf (XStruct kvps) = inferTypeOf' kvps >>= tryUpdateDAG . TStruct
  where
    inferTypeOf' ((key, e) : tl) = do
      id_e <- inferTypeOf e
      tl' <- inferTypeOf' tl
      return $ (key, id_e) : tl'
    inferTypeOf' [] = return []
inferTypeOf (XVar name) = 
  tryFindID name >>= \case
    Just id' -> return id'
    Nothing -> throwError "variable is not declared"
inferTypeOf (XProj e field) = do
  t <- inferTypeOf e >>= findByID
  case t of
    TStruct kvps -> case find ((field ==) . fst) kvps of
      Just (_, v) -> return v
      Nothing -> return topType
    _ -> throwError "t is not a struct"
inferTypeOf (XIndex e1 e2) = do
  t <- inferTypeOf e1 >>= findByID
  i <- inferTypeOf e2 >>= findByID
  case (t, i) of
    (TArray n t' cs, TInt i') -> undefined
    _ -> throwError "t is not an array or i' is not an int type"
inferTypeOf (XCall name as) = do
  id_f <- findTypeID name
  as' <- inferTypeOfMany as
  call id_f as'

inferTypeOfMany :: [Expr] -> EnvironmentP [ID]
inferTypeOfMany = foldl (\a x -> liftM2 (:) (inferTypeOf x) a) (pure [])

inferTypeOfBinop :: Expr -> Expr -> ID -> EnvironmentP ID
inferTypeOfBinop e1 e2 id_f = do
  id_1 <- inferTypeOf e1
  id_2 <- inferTypeOf e2
  call id_f [id_1, id_2]

-- isSingleton :: TypeExpr -> EnvironmentP Bool
-- isSingleton (TInt (INumber _)) = return True
-- isSingleton (TBool (BValue _)) = return True
-- isSingleton (TUnion t1 t2) = return False
-- isSingleton (TStruct kvps) = case kvps of
--   [] -> return True
--   (_, t) : tl -> liftM2 (&&) (isSingleton t) (isSingleton $ TStruct tl)
-- isSingleton (TArray n t annotations) = liftM2 (||) (isSingleton t)
--   (fmap (length annotations == n &&) (checkAnnotations $ map snd annotations))
--   where
--     checkAnnotations [] = return True
--     checkAnnotations (id' : tl) = do
--       t' <- findByID id'
--       liftM2 (&&) (isSingleton t') (checkAnnotations tl)
-- isSingleton _ = return False

initializeAll :: EnvironmentP ()
initializeAll = undefined

eqType :: ID
eqType = undefined

gtType :: ID
-- gtType = TMap [TInt, TInt] TBool
gtType = undefined

ltType :: ID
ltType = undefined

andType :: ID
andType = undefined

orType :: ID
orType = undefined

notType :: ID
notType = undefined

addType :: ID
addType = undefined

subtractType :: ID
subtractType = undefined

intType :: ID
intType = undefined

bottomType :: ID
bottomType = undefined

topType :: ID
topType = undefined

boolType :: ID
boolType = undefined

-- unitType :: TypeExpr
-- unitType = TUnit

call :: ID -> [ID] -> EnvironmentP ID
call id_f id_as = do
  f <- findByID id_f
  case f of
    TMap id_ps id_rt -> do
      checkParams id_ps id_as
      return id_rt
    _ -> throwError "call only accepts mappings"
  where
    checkParams (p : ps) (a : as) = do
      glb <- intersect [p, a]
      absurdityGuard glb
      a `isSubtypeOf` p >>= guard
      checkParams ps as
    checkParams [] [] = return ()
    checkParams _ _ = throwError "parameter arity mismatch"

isSubtypeOf :: ID -> ID -> EnvironmentP Bool
isSubtypeOf t1 t2 = isConnected t1 t2 <$> getDAG

-- eqTypingRule :: Arity2
-- eqTypingRule t1 t2 = if t1 `equals` t2 then TTrue else TBool

-- gtTypingRule :: Arity2
-- gtTypingRule t1 t2 =
--   if (t1 `isSubtypeOf` TInt) && (t2 `isSubtypeOf` TInt) && (getMax t1 < getMin t2)
--   then TTrue
--   else TBool

-- ltTypingRule :: Arity2
-- ltTypingRule t1 t2 =
--   if (t1 `isSubtypeOf` TInt) && (t2 `isSubtypeOf` TInt) && (getMax t2 < getMin t1)
--   then TTrue
--   else TBool

-- readonlyRule :: Arity2
-- readonlyRule = undefined

getMinInt :: TypeInt -> EnvironmentP Int
getMinInt (IRange i1 _) = return i1
getMinInt (INumber i) = return i
getMinInt IInteger = return minBound
getMinInt (IUnion i1 _) = getMinInt i1

getMaxInt :: TypeInt -> EnvironmentP Int
getMaxInt (IRange _ i2) = return i2
getMaxInt (INumber i) = return i
getMaxInt IInteger = return maxBound
getMaxInt (IUnion _ i2) = getMaxInt i2

absurdityGuard :: ID -> EnvironmentP ()
absurdityGuard t = when (t == bottomType) $ throwError "no suitable type"

typeCheck :: Program -> EnvironmentP ()
typeCheck = foldr ((>>) . typeCheck') $ pure ()

typeCheck' :: Statement -> EnvironmentP ()
typeCheck' (SAssign var e) = do
  id_left <- inferTypeOf (XVar var)
  id_right <- inferTypeOf e
  glb <- intersect [id_left, id_right]
  absurdityGuard glb
  setVarToTypeID var glb
  case e of
    XVar e' -> setVarToTypeID e' glb
    _ -> return ()
typeCheck' (SImpure e) = do
  t <- inferTypeOf e
  absurdityGuard t
typeCheck' _ = undefined
