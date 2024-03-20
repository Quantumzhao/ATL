{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
inferTypeOf XUnit = return unitType
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
inferTypeOf (XSymbol name) = 
  tryFindID name >>= \case
    Just id' -> return id'
    Nothing -> throwError "variable is not declared"
inferTypeOf (XProj e key) = do
  t <- inferTypeOf e >>= findByID
  case t of
    TStruct kvps -> case find ((key ==) . fst) kvps of
      Just (_, v) -> return v
      Nothing -> return topType
    _ -> throwError "t is not a struct"
inferTypeOf (XIndex e1 e2) = do
  t <- inferTypeOf e1 >>= findByID
  i <- inferTypeOf e2 >>= findByID
  case (t, i) of
    (TArray n t' cs, TInt i') -> 
      -- not exactly the LUB, but should be enough
      case tryNarrowDownTInt 0 n i' of
        Just (INumber i'') ->
          case find ((== i'') . fst) cs of
            Just (_, t'') -> return t''
            Nothing -> return t'
        Just _ -> return t'
        Nothing -> throwError "index out of bound"
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

-- assuming i1 < i2
tryNarrowDownTInt :: Int -> Int -> TypeInt -> Maybe TypeInt
tryNarrowDownTInt i1 i2 IInteger = Just $ IRange i1 i2
tryNarrowDownTInt i1 i2 (IRange i1' i2') = 
  let (i1'', i2'') = (max i1 i1', min i2 i2') in
  if i1'' < i2'' then Just $ IRange i1'' i2''
  else Nothing
tryNarrowDownTInt i1 i2 (INumber i) = if i1 <= i && i <= i2 then Just $ INumber i else Nothing
tryNarrowDownTInt i1 i2 (IUnion i1' i2') = do
  i1'' <- tryNarrowDownTInt i1 i2 i1'
  i2'' <- tryNarrowDownTInt i1 i2 i2'
  return $ IUnion i1'' i2''

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
eqType = 1

gtType :: ID
-- gtType = TMap [TInt, TInt] TBool
gtType = 2

ltType :: ID
ltType = 3

andType :: ID
andType = 4

orType :: ID
orType = 5

notType :: ID
notType = 6

addType :: ID
addType = 7

subtractType :: ID
subtractType = 8

intType :: ID
intType = 9

bottomType :: ID
bottomType = 10

topType :: ID
topType = 11

boolType :: ID
boolType = 12

unitType :: ID
unitType = 13

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
      res <- tryMoveBelow a p
      if res then checkParams ps as
      else throwError "no casting from argument to a subtype of p"
    checkParams [] [] = return ()
    checkParams _ _ = throwError "parameter arity mismatch"

isSubtypeOf :: ID -> ID -> EnvironmentP Bool
isSubtypeOf t1 t2 = isConnected t1 t2 <$> getDAG

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

typeCoercion :: ID -> ID -> EnvironmentP ID
typeCoercion orig constraint = do
  glb <- intersect [orig, constraint]
  absurdityGuard glb
  return glb

typeCheck :: Program -> EnvironmentP (Signal ID)
typeCheck [] = return SigContinue
typeCheck (stmt : rest) = do
  sig <- typeCheck' stmt
  case sig of
    SigContinue -> typeCheck rest
    _ -> return sig

typeCheck' :: Statement -> EnvironmentP (Signal ID)
typeCheck' (SAssign var e) = do
  id_right <- inferTypeOf e
  absurdityGuard id_right
  -- setVarToTypeID var id_right
  
  return SigContinue
typeCheck' (SImpure e) = inferTypeOf e >>= absurdityGuard >> return SigContinue
typeCheck' SBreak = return SigBreak
typeCheck' (SReturn expr) = SigReturn <$> inferTypeOf expr
typeCheck' (SIf be if_true if_false) = do
  id_bt <- inferTypeOf be
  glb <- typeCoercion id_bt boolType
  undefined
typeCheck' (SWhile expr body) = undefined
typeCheck' (SProcedure (name, ps, body)) = undefined
typeCheck' (SSwitch expr cs) = do
  t_expr <- inferTypeOf expr
  checkCases t_expr cs
  return SigContinue
  where
    checkCases _ [] = return ()
    checkCases t_expr cs = undefined

