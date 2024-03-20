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

inferTypeOf :: Expr -> EnvironmentP (ID, [VarExpr])
inferTypeOf (XEqual e1 e2) = inferTypeOfBinop e1 e2 eqType
inferTypeOf (XGt e1 e2) = inferTypeOfBinop e1 e2 gtType
inferTypeOf (XLt e1 e2) = inferTypeOfBinop e1 e2 ltType
inferTypeOf (XAnd e1 e2) = inferTypeOfBinop e1 e2 andType
inferTypeOf (XOr e1 e2) = inferTypeOfBinop e1 e2 orType
inferTypeOf (XNot e) = do
  (ids, vars) <- (: []) <$$> inferTypeOf e
  removeConstraintsOn' vars
  call notType ids <!> vars
inferTypeOf (XAdd e1 e2) = inferTypeOfBinop e1 e2 addType
inferTypeOf (XSub e1 e2) = inferTypeOfBinop e1 e2 subtractType
inferTypeOf (XInt i) = pureOp $ tryUpdateDAG (TInt $ INumber i)
inferTypeOf (XBool b) = pureOp $ tryUpdateDAG (TBool $ BValue b)
inferTypeOf XUnit = return (unitType, [])
inferTypeOf (XArray es) = do
  (id_ts, mutables) <- inferTypeOfMany es
  id_lub <- union id_ts
  extra <- locateSpecificTypes id_lub id_ts 0
  ids <- tryUpdateDAG $ TArray (length es) id_lub extra
  return (ids, mutables)
  where
    locateSpecificTypes glb (t : ts) curr = do
      res <- t <-< glb
      ret <- locateSpecificTypes glb ts (curr + 1)
      if res then return $ (curr, t) : ret
      else return ret
    locateSpecificTypes _ [] _ = return []
inferTypeOf (XStruct kvps) = do
  (t_kvps, muts) <- inferTypeOf' kvps
  id <- tryUpdateDAG $ TStruct t_kvps
  return (id, muts)
  where
    inferTypeOf' ((key, e) : tl) = do
      (id_e, muts_e) <- inferTypeOf e
      (tl', muts_tl) <- inferTypeOf' tl
      return ((key, id_e) : tl', muts_e <> muts_tl)
    inferTypeOf' [] = return ([], [])
inferTypeOf (XSymbol name) =
  tryFindID name >>= \case
    Just id' -> return (id', [])
    Nothing -> throwError "VarExpr is not declared"
inferTypeOf (XProj e key) = do
  (t, muts) <- inferTypeOf e
  t' <- findByID t
  case t' of
    TStruct kvps -> case find ((key ==) . fst) kvps of
      Just (_, v) -> pureOp $ return v
      Nothing -> pureOp $ return topType
    _ -> throwError "t is not a struct"
inferTypeOf (XIndex e1 e2) = do
  (t, vars_t) <- inferTypeOf e1 `through` findByID
  (i, vars_i) <- inferTypeOf e2 `through` findByID
  case (t, i) of
    (TArray n t' cs, TInt i') ->
      -- not exactly the LUB, but should be enough
      case tryNarrowDownTInt 0 n i' of
        Just (INumber i'') ->
          case find ((== i'') . fst) cs of
            Just (_, t'') -> return (t'', vars_t <> vars_i)
            Nothing -> return (t', vars_t <> vars_i)
        Just _ -> return (t', vars_t <> vars_i)
        Nothing -> throwError "index out of bound"
    _ -> throwError "t is not an array or i' is not an int type"
inferTypeOf (XCall name as) = do
  id_f <- findTypeID name
  (as', muts) <- inferTypeOfMany as
  call id_f as'

pureOp :: Monad m => m t -> m (t, [VarExpr])
pureOp ret = (,) <$> ret <*> pure []

(<$$>) :: Monad m => (a -> c) -> m (a, b) -> m (c, b)
(<$$>) f = (=<<) (\(a, b) -> return (f a, b))

(<!>) :: Monad m => m (a, [b]) -> [b] -> m (a, [b])
m <!> b = (\(a, b') -> (a, b' <> b)) <$> m

(<:>) :: Monad m => m (a, [b]) -> m ([a], [b]) -> m ([a], [b])
a <:> b = (\(a1, b1) (a2, b2) -> (a1 : a2, b1 <> b2)) <$> a <*> b

through :: Monad m => m (a, [VarExpr]) -> (a -> m b) -> m (b, [VarExpr])
through env f = do
  (ret, vars) <- env
  ret' <- f ret
  return (ret', vars)

inferTypeOfMany :: [Expr] -> EnvironmentP ([ID], [VarExpr])
inferTypeOfMany = foldl (\a x -> inferTypeOf x <:> a) (pure ([], []))

inferTypeOfBinop :: Expr -> Expr -> ID -> EnvironmentP (ID, [VarExpr])
inferTypeOfBinop e1 e2 id_f = do
  (id_1, vars_1) <- inferTypeOf e1
  removeConstraintsOn' vars_1
  (id_2, vars_2) <- inferTypeOf e2
  removeConstraintsOn' vars_2
  call id_f [id_1, id_2] <!> vars_1 <!> vars_2

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

call :: ID -> [ID] -> EnvironmentP (ID, [VarExpr])
call id_f id_as = do
  f <- findByID id_f
  case f of
    Nothing -> do
      newId <- genUnknownType
      let t_newMap = TMap id_as newId []
      id_newMap <- tryUpdateDAG t_newMap
      undefined
      -- (,) <$> (id_f >>= id_newMap) <*> pure []
    Just (TMap id_ps id_rt vars) -> do
      checkParams id_ps id_as
      return (id_rt, vars)
    _ -> throwError "call only accepts mappings"
  where
    checkParams (p : ps) (a : as) = do
      res <- tryMoveBelow a p
      if res then checkParams ps as
      else throwError "no casting from argument to a subtype of p"
    checkParams [] [] = return ()
    checkParams _ _ = throwError "parameter arity mismatch"

-- isSubtypeOf :: ID -> ID -> EnvironmentP Bool
-- isSubtypeOf t1 t2 = isConnected t1 t2 <$> getDag

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

-- typeCoercion :: ID -> ID -> EnvironmentP ID
-- typeCoercion orig constraint = do
--   glb <- intersect [orig, constraint]
--   absurdityGuard glb
--   return glb

(>==) :: ID -> ID -> EnvironmentP ID
id_1 >== id_2 = do
  (,) <$> findByID id_1 <*> findByID id_2 >>= \case
    (Nothing, _) -> tryMoveBelow id_1 id_2 >> return id_1
    (Just t_1, Just t_2) -> undefined
    (Just t_1, Nothing) -> undefined

typeCheck :: Program -> EnvironmentP (Signal ID, [VarExpr])
typeCheck [] = pureOp $ return SigContinue
typeCheck (stmt : rest) = do
  (sig, vars) <- typeCheck' stmt
  case sig of
    SigContinue -> typeCheck rest
    _ -> return (sig, vars)

typeCheck' :: Statement -> EnvironmentP (Signal ID, [VarExpr])
typeCheck' (SAssign var e) = do
  (id_right, vars_right) <- inferTypeOf e
  absurdityGuard id_right
  -- setVarToTypeID var id_right

  pureOp $ return SigContinue
typeCheck' (SImpure e) = const SigContinue <$$>
  (let e' = inferTypeOf e in e' `through` absurdityGuard >> e')
typeCheck' SBreak = pureOp $ return SigBreak
typeCheck' (SReturn expr) = SigReturn <$$> inferTypeOf expr
typeCheck' (SIf be if_true if_false) = do
  (id_bt, vars_bt) <- inferTypeOf be
  removeConstraintsOn' vars_bt
  glb <- id_bt >== boolType
  absurdityGuard glb
  if glb == trueType then
    typeCheck if_true <!> vars_bt
  else if glb == falseType then
    typeCheck if_false <!> vars_bt
  else undefined
typeCheck' (SWhile expr body) = undefined
typeCheck' (SProcedure (name, ps, body)) = undefined
typeCheck' (SSwitch expr cs) = do
  t_expr <- inferTypeOf expr
  checkCases t_expr cs
  pureOp $ return SigContinue
  where
    checkCases _ [] = return ()
    checkCases t_expr cs = undefined


