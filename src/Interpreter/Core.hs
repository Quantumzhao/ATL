module Interpreter.Core
  ( eval
  , evalM
  , execute
  , executeMany )
  where

import Types
import Interpreter.Context
  ( Environment
  , ProgramState
  , doesExistVar
  , findVar
  , findProc
  , modifyVars, discardClosure )
import Control.Monad.State( get , put , modify , evalStateT , guard )
import Control.Monad.Except( throwError )
import Control.Monad ( liftM2 )
import Data.Functor ( (<&>) )
import Data.Bifunctor ( second )
import Data.List (find)
import Data.List.Extra ( (!?) )

eval :: Environment -> Expr -> Unchecked Value
eval env expr = evalStateT (evalM expr) env

evalM :: Expr -> ProgramState Value
evalM (XInt i) = return $ Int i
evalM (XBool b) = return $ Bool b
-- evalM XUnit = return Unit
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
evalM (XVar var) = do
  m <- findVar var
  case m of
    Just v -> return v
    Nothing -> throwError $ var <> " is not declared"
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

execute :: Statement -> ProgramState Signal
execute (SAssign name expr) = do
  v <- evalM expr
  b <- doesExistVar name
  if b then
    modifyVars (map (\kvp@(name', _) -> if name' == name then (name, v) else kvp)) >>
    return SigContinue
  else modifyVars ((name, v) :) >> return SigContinue
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
    -- (Unit, PValue (MatchV mx)) -> evalM mx >>= assert v
    -- (Unit, PValue (BindV u')) -> expand u' v
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
  checkArray (v : rest) (CheckA p aps) = liftM2 (&&) (checkPattern v p) (checkArray rest aps)
  checkArray (_ : _) (SkipSomeA EmptyA) = return True
  checkArray (_ : _ : rest) (SkipSomeA (SkipSomeA aps)) = checkArray rest aps
  checkArray (_ : v : rest) sa@(SkipSomeA (CheckA p as)) = do
    res <- checkPattern v p
    if res then checkArray rest as
    else checkArray rest sa
  checkArray _ _ = return False
  expand :: Variable -> Value -> ProgramState Bool
  expand name v = modifyVars ((name, v) :) >> return True
  assert value match = return (value == match)
execute (SProcedure p) = modify (second (p :)) >> return SigContinue

executeMany :: [Statement] -> ProgramState Signal
executeMany [] = return $ SigReturn Null
executeMany (stmt : rest) = do
  sig <- execute stmt
  case sig of
    SigContinue -> executeMany rest
    _ -> return sig
