{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpreter.Core
  ( eval
  , evalM
  , execute
  , executeMany
  , execWithEnv )
  where

import Types
import Interpreter.Context
  ( Environment
  , ProgramState
  , doesExistVar
  , findVar
  , findProc
  , modifyVars, discardClosure, getVars
  , putVars )
import Control.Monad.State( get , put , modify , evalStateT , runStateT , guard )
import Control.Monad.Except( throwError , runExcept )
import Control.Monad ( liftM2 )
import Data.Functor ( (<&>) )
import Data.Bifunctor ( second )
import Data.List (find)
import Data.List.Extra ( (!?) )

-- given x / x.y / x[i] and return the state of x and value after set
modifyVar :: (a -> Bool) -> (a -> a) -> [a] -> [a]
modifyVar discriminator f vars =
  foldl (\vars' v ->
    if discriminator v then f v : vars
    else v : vars
  ) [] vars

eval :: Environment -> Expr -> Unchecked Value
eval env expr = evalStateT (evalM expr) env

evalM :: Expr -> ProgramState Value
evalM (XInt i) = return $ Int i
evalM (XBool b) = return $ Bool b
evalM XUnit = return Unit
-- evalM (XProc ps b) = return $ Proc ps b
evalM (XAdd e1 e2) = do
  v1 <- evalM e1
  v2 <- evalM e2
  case (v1, v2) of
    (Int i1, Int i2) -> return $ Int $ i1 + i2
    (_, _) -> throwError "type error: Add"
evalM (XSub e1 e2) = do
  v1 <- evalM e1
  v2 <- evalM e2
  case (v1, v2) of
    (Int i1, Int i2) -> return $ Int $ i1 - i2
    (_, _) -> throwError "type error: Subtract"
evalM (XNot e) = do
  v <- evalM e
  case v of
    (Bool b) -> return $ Bool $ not b
    _ -> throwError "type error: Not"
evalM (XOr e1 e2) = do
  v1 <- evalM e1
  v2 <- evalM e2
  case (v1, v2) of
    (Bool b1, Bool b2) -> return $ Bool $ b1 || b2
    (_, _) -> throwError "type error: Or"
evalM (XAnd e1 e2) = do
  v1 <- evalM e1
  v2 <- evalM e2
  case (v1, v2) of
    (Bool b1, Bool b2) -> return $ Bool $ b1 && b2
    (_, _) -> throwError "type error: And"
evalM (XEqual e1 e2) = do
  v1 <- evalM e1
  v2 <- evalM e2
  return $ Bool $ v1 == v2
evalM (XGt e1 e2) = do
  v1 <- evalM e1
  v2 <- evalM e2
  case (v1, v2) of
    (Int i1, Int i2) -> return $ Bool $ i1 > i2
    (_, _) -> throwError "type error: GreaterThan"
evalM (XLt e1 e2) = do
  v1 <- evalM e1
  v2 <- evalM e2
  case (v1, v2) of
    (Int i1, Int i2) -> return $ Bool $ i1 > i2
    (_, _) -> throwError "type error: GreaterThan"
evalM (XArray as) = do
  vs <- evalArr as
  return $ Array vs
  where
    evalArr [] = return []
    evalArr (hd : tl) = do
      v <- evalM hd
      vs <- evalArr tl
      return $ v : vs
evalM (XStruct s) =
  evalS s <&> Struct
  where
    evalS [] = return []
    evalS ((name, hd) : tl) = do
      v <- evalM hd
      vs <- evalS tl
      return $ (name, v) : vs
evalM (XSymbol name) = do
  var <- findVar name
  case var of
    Just v -> return v
    Nothing -> throwError $ name <> " is not declared"
evalM (XProj var1 var2) = do
  var1' <- evalM var1
  case var1' of
    Struct kvps -> case find ((var2 ==) . fst) kvps of
                     Just (k, v) -> return v
                     Nothing -> throwError " field not found "
    _ -> throwError "not a struct"
evalM (XIndex var i) = do
  var' <- evalM var
  i' <- evalM i
  case (var', i') of
    (Array vs, Int i'') -> case vs !? i'' of
                             Just v -> return v
                             Nothing -> throwError "index out of bound"
    _ -> throwError "type mismatch"
evalM (XCall fname exprs) = do
  m <- findProc fname
  args <- evalArgs exprs
  case m of
    Just (_, params, body) -> do
        modifyVars $ \x -> zip params args <> x
        sig <- executeMany body
        case sig of
          SigBreak -> throwError "function terminated by break statement"
          SigReturn x -> return x
          SigContinue -> throwError "Wait that's illegal"
    _ -> throwError $ fname <> " not found"
  where
  evalArgs [] = return []
  evalArgs (expr : rest) = do
    v <- evalM expr
    vs <- evalArgs rest
    return $ v : vs

execute :: Statement -> ProgramState (Signal Value)
execute (SAssign var expr) = do
  v <- evalM expr
  vars <- getVars
  subst <- substitute var (\v' v -> v)
  case subst v of
    Struct vars'' -> putVars vars'' >> return SigContinue
    _ -> throwError "execute: how could this possibly happen?"
  where
    -- returns domain -> substitution -> new domain
    substitute :: VarExpr -> (Value -> Value -> Value) -> ProgramState (Value -> Value)
    substitute (VSymbol name) f_subst = do
      vars <- getVars
      return $ \v ->
        Struct $ replaceOrInsert name v f_subst vars
    substitute (VIndex arr index) f_subst = do
      i <- evalM index
      substitute arr (\domain v -> case (domain, i) of
        (Array es, Int i') -> Array $ map snd $ replaceOrInsert i' v f_subst (zip [0..] es)
        _ -> error "can't index non-array values or index is not int")
    substitute (VProj dict key) f_subst =
      substitute dict (\domain v -> case domain of
        Struct kvps -> Struct $ replaceOrInsert key v f_subst kvps
        _ -> error "")
    replaceOrInsert key x f [] = [(key, x)]
    replaceOrInsert key' x f ((key, hd) : tl) =
      if key' == key then (key, f hd x) : tl else (key, hd) : replaceOrInsert key' x f tl
execute (SIf c tbranch fbranch) = do
  v <- evalM c
  env <- get
  case v of
    Bool b -> if b then do discardClosure $ executeMany tbranch
              else discardClosure $ executeMany fbranch
    _ -> throwError "expect bool; type mismatch in If"
execute (SWhile c body) = do
  v <- evalM c
  case v of
    Bool b -> if b then do
                sig <- executeMany body
                case sig of
                  SigBreak -> return SigContinue
                  SigContinue -> execute (SWhile c body)
                  _ -> return sig
              else return SigContinue
    _ -> throwError "expect bool; type mismatch in While"
execute (SReturn x) = evalM x <&> SigReturn
execute SBreak = return SigBreak
execute (SImpure expr) = evalM expr >> return SigContinue
execute (SSwitch expr cs) = do
  v <- evalM expr
  checkCases v cs
  where
  checkPattern v ptn = case (v, ptn) of
    (Int _, PValue (MatchV mx)) -> evalM mx >>= assert v
    (Int _, PValue (BindV i')) -> expand i' v
    (Bool _, PValue (MatchV mx)) -> evalM mx >>= assert v
    (Bool _, PValue (BindV b')) -> expand b' v
    (Struct kvps, PStruct rps) -> checkRecord kvps rps
    (Array a, PArray aps) -> checkArray a aps
    _ -> return False
  checkCases _ [] = return SigContinue
  checkCases v ((ptn, body) : rest) = do
    b <- checkPattern v ptn
    if b then executeMany body
    else checkCases v rest
  checkRecord kvps ((key, ptn) : rest) =
    case find (\kvp -> fst kvp == key) kvps of
      Just (_, v) -> liftM2 (&&) (checkPattern v ptn) (checkRecord kvps rest)
      Nothing -> return False
  checkRecord _ [] = return False
  checkArray [] EmptyA = return True
  checkArray (v : rest) (CheckA p aps) = (&&) <$> checkPattern v p <*> checkArray rest aps
  checkArray (_ : _) (SkipSomeA EmptyA) = return True
  checkArray (_ : _ : rest) (SkipSomeA (SkipSomeA aps)) = checkArray rest aps
  checkArray (_ : v : rest) sa@(SkipSomeA (CheckA p as)) = do
    res <- checkPattern v p
    if res then checkArray rest as
    else checkArray rest sa
  checkArray _ _ = return False
  expand name v = modifyVars ((name, v) :) >> return True
  assert value match = return (value == match)
execute (SProcedure p) = modify (second (p :)) >> return SigContinue

executeMany :: [Statement] -> ProgramState (Signal Value)
executeMany [] = return $ SigReturn Unit
executeMany (stmt : rest) = do
  sig <- execute stmt
  case sig of
    SigContinue -> executeMany rest
    _ -> return sig

execWithEnv ::  Environment -> Program -> Either String (Signal Value, Environment)
execWithEnv env prog = runExcept $ runStateT (executeMany prog) env
