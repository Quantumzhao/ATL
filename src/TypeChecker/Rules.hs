{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TypeChecker.Rules
( inferTypeOf

)
where

import Types
import TypeChecker.TypingContext
import Control.Monad.Except ( throwError )
import Control.Monad ( liftM2 , guard )

-- toTypeBool :: Bool -> TypeBool
-- toTypeBool True = TypeTrue
-- toTypeBool False = TypeFalse

inferTypeOf :: Expr -> EnvironmentP TypeExpr
inferTypeOf (XEqual e1 e2) = do
  t1 <- inferTypeOf e1
  t2 <- inferTypeOf e2
  findByID eqType >>= call [t1, t2]
inferTypeOf (XGt e1 e2) = do
  id1 <- inferTypeOf e1
  id2 <- inferTypeOf e2
  findByID gtType >>= call [id1, id2]
inferTypeOf (XLt e1 e2) = do
  id1 <- inferTypeOf e1
  id2 <- inferTypeOf e2
  findByID ltType >>= call [id1, id2]
inferTypeOf (XAnd e1 e2) = do
  id1 <- inferTypeOf e1
  id2 <- inferTypeOf e2
  findByID andType >>= call [id1, id2]
inferTypeOf (XOr e1 e2) = do
  id1 <- inferTypeOf e1
  id2 <- inferTypeOf e2
  findByID orType >>= call [id1, id2]
inferTypeOf (XNot e) = do
  id1 <- inferTypeOf e
  findByID notType >>= call [id1]
inferTypeOf (XAdd e1 e2) = do
  id1 <- inferTypeOf e1
  id2 <- inferTypeOf e2
  findByID addType >>= call [id1, id2]
inferTypeOf (XSub e1 e2) = do
  id1 <- inferTypeOf e1
  id2 <- inferTypeOf e2
  findByID subtractType >>= call [id1, id2]
inferTypeOf (XInt i) = return $ TInt $ INumber i
inferTypeOf (XBool b) = return $ TBool $ BValue b
-- inferTypeOf Null = return TTop
inferTypeOf (XArray es) = do
  ids <- inferTypeOfMany es >>= tryUpdateDAGMany
  lub <- findLub ids
  lub' <- findByID lub
  extra <- locateSpecificTypes lub ids 0
  return $ TArray (length es) lub' extra
  where
    locateSpecificTypes glb (t : ts) curr = do
      res <- t `strictlyGt` glb
      ret <- locateSpecificTypes glb ts (curr + 1)
      if res then return $ (curr, t) : ret
      else return ret
    locateSpecificTypes _ [] _ = return []
inferTypeOf (XStruct kvps) = TStruct <$> inferTypeOf' kvps
  where
    inferTypeOf' ((key, e) : tl) = do
      e' <- inferTypeOf e
      tl' <- inferTypeOf' tl
      return $ (key, e') : tl'
    inferTypeOf' [] = return []
inferTypeOf (XVar name) = findType name >>= findByID
inferTypeOf (XCall name as) = do
  t <- findType name >>= findByID
  as' <- inferTypeOfMany as
  call as' t
-- inferTypeOf (XProc ps body) = undefined

inferTypeOfMany :: [Expr] -> EnvironmentP [TypeExpr]
inferTypeOfMany = foldl (\a x -> liftM2 (:) (inferTypeOf x) a) (pure [])

isSingleton :: TypeExpr -> EnvironmentP Bool
isSingleton (TInt (INumber _)) = return True
isSingleton (TBool (BValue _)) = return True
isSingleton (TUnion t1 t2) = liftM2 (&&) (isSingleton t1) (isSingleton t2)
isSingleton (TStruct kvps) = case kvps of
  [] -> return True
  (_, t) : tl -> liftM2 (&&) (isSingleton t) (isSingleton $ TStruct tl)
isSingleton (TArray n t annotations) = liftM2 (||) (isSingleton t)
  (fmap (length annotations == n &&) (checkAnnotations $ map snd annotations))
  where
    checkAnnotations [] = return True
    checkAnnotations (id' : tl) = do
      t' <- findByID id'
      liftM2 (&&) (isSingleton t') (checkAnnotations tl)
isSingleton _ = return False

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

boolType :: TypeExpr
boolType = undefined

-- unitType :: TypeExpr
-- unitType = TUnit

call :: [TypeExpr] -> TypeExpr -> EnvironmentP TypeExpr
call ids (TMap ps rt) = do
  pts' <- tryUpdateDAGMany ps
  ids' <- tryUpdateDAGMany ids
  checkParams pts' ids' >>= guard
  return rt
  where
    checkParams (pt : pts) (at : ats) = do
      at `isSubtypeOf` pt >>= guard
      checkParams pts ats
    checkParams [] [] = return True
    checkParams _ _ = return False
call _ _ = throwError "call only accepts mappings"

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

typeCheck :: Program -> EnvironmentP ()
typeCheck = undefined
