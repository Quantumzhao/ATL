module Interpreter.Core
(
    eval,
    evalM,
    execute,
    executeMany,
    -- run
)
where

import Interpreter.Types
import Interpreter.Assumptions( Environment , ContextP , doesExist , findByNameM , find )
import Control.Monad.State( get, modify , evalStateT )
import Control.Monad.Except( throwError )
import Control.Monad ( liftM2 )

eval :: Environment -> Expr -> Unchecked Value
eval env expr = evalStateT (evalM expr) env

evalM :: Expr -> ContextP Value
evalM (Literal l) = return l
evalM (Add e1 e2) = do
    v1 <- evalM e1
    v2 <- evalM e2
    case (v1, v2) of
        (Int i1, Int i2) -> return $ Int $ i1 + i2
        (_, _) -> throwError "type error: Add"
evalM (Subtract e1 e2) = do
    v1 <- evalM e1
    v2 <- evalM e2
    case (v1, v2) of
        (Int i1, Int i2) -> return $ Int $ i1 - i2
        (_, _) -> throwError "type error: Subtract"
evalM (Not e) = do
    v <- evalM e
    case v of
        (Bool b) -> return $ Bool $ not b
        _ -> throwError "type error: Not"
evalM (Or e1 e2) = do
    v1 <- evalM e1
    v2 <- evalM e2
    case (v1, v2) of
        (Bool b1, Bool b2) -> return $ Bool $ b1 || b2
        (_, _) -> throwError "type error: Or"
evalM (And e1 e2) = do
    v1 <- evalM e1
    v2 <- evalM e2
    case (v1, v2) of
        (Bool b1, Bool b2) -> return $ Bool $ b1 && b2
        (_, _) -> throwError "type error: And"
evalM (Equal e1 e2) = do
    v1 <- evalM e1
    v2 <- evalM e2
    return $ Bool $ v1 == v2
evalM (GreaterThan e1 e2) = do
    v1 <- evalM e1
    v2 <- evalM e2
    case (v1, v2) of
        (Int i1, Int i2) -> return $ Bool $ i1 > i2
        (_, _) -> throwError "type error: GreaterThan"
evalM (LessThan e1 e2) = do
    v1 <- evalM e1
    v2 <- evalM e2
    case (v1, v2) of
        (Int i1, Int i2) -> return $ Bool $ i1 > i2
        (_, _) -> throwError "type error: GreaterThan"
evalM (ArrayExpr as) = do
    vs <- evalArr as
    return $ Array $ vs
    where
        evalArr [] = return []
        evalArr (hd : tl) = do
            v <- evalM hd
            vs <- evalArr tl
            return $ v : vs
-- evalM (LinkedListExpr ls) = do
--     vs <- evalLL ls
--     return $ LinkedList $ vs
--     where
--         evalLL [] = return []
--         evalLL (hd : tl) = do
--             v <- evalM hd
--             vs <- evalLL tl
--             return $ v : vs
evalM (RecordExpr s) = 
    evalS s >>= return . Record
    where
        evalS [] = return []
        evalS ((name, hd) : tl) = do
            v <- evalM hd
            vs <- evalS tl
            return $ (name, v) : vs
evalM (Variable var) = findByNameM var
evalM (Call fname exprs) = do
    f <- findByNameM fname
    args <- evalArgs exprs
    case f of
        Function params body -> do
            modify $ \x -> (fname, f) : (zip params args) <> x
            sig <- executeMany body
            case sig of
                SigBreak -> throwError "function terminated by break statement"
                SigReturnX x -> return x
                SigReturn -> return Unit -- invalid when the expr is the right value; 
                                         -- type checker'll handle this
                SigContinue -> throwError "Wait that's illegal"
        _ -> throwError $ fname <> " not found"
    where
    evalArgs [] = return []
    evalArgs (expr : rest) = do
        v <- evalM expr
        vs <- evalArgs rest
        return $ v : vs

execute :: Statement -> ContextP Signal
execute (AssignDefine name expr) = do
    env <- get
    v <- evalM expr
    if doesExist env name then throwError $ name <> " is already defined"
    else modify ((name, v) :) >> return SigContinue
execute (Assign name expr) = do
    env <- get
    v <- evalM expr
    if doesExist env name then
        modify (map (\kvp@(name', _) -> if name' == name then (name, v) else kvp))
        >> return SigContinue
    else throwError $ name <> " doesn't exist"
execute (If c tbranch fbranch) = do
    v <- evalM c
    case v of
        Bool b -> if b then executeMany tbranch
                  else executeMany fbranch
        _ -> throwError "expect bool; type mismatch in If"
execute (While c body) = do
    v <- evalM c
    case v of
        Bool b -> if b then do
                      sig <- executeMany body
                      case sig of
                          SigBreak -> return SigContinue
                          SigContinue -> execute (While c body)
                          _ -> return sig
                  else return SigContinue
        _ -> throwError "expect bool; type mismatch in While"
execute (ReturnX x) = (evalM x) >>= return . SigReturnX
execute Return = return SigReturn
execute Break = return SigBreak
execute (Impure expr) = evalM expr >> return SigContinue
execute (Switch expr cs) = do
    v <- evalM expr
    checkCases v cs
    where
    checkPattern v ptn = case (v, ptn) of
        (Int _, ValueP (MatchV mx)) -> evalM mx >>= assert v
        (Int _, ValueP (BindV i')) -> expand i' v
        (Bool _, ValueP (MatchV mx)) -> evalM mx >>= assert v
        (Bool _, ValueP (BindV b')) -> expand b' v
        (Unit, ValueP (MatchV mx)) -> evalM mx >>= assert v
        (Unit, ValueP (BindV u')) -> expand u' v
        (Record kvps, RecordP rps) -> checkRecord kvps rps
        (Array a, ArrayP aps) -> checkArray a aps
        _ -> return False
    checkCases _ [] = return SigContinue
    checkCases v ((ptn, body) : rest) = do
        b <- checkPattern v ptn
        if b then executeMany body
        else checkCases v rest 
    checkRecord kvps ((key, ptn) : rest) = 
        case find kvps (\kvp -> fst kvp == key) of
            Just v -> liftM2 (&&) (checkPattern v ptn) (checkRecord kvps rest)
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
    expand :: Variable -> Value -> ContextP Bool
    expand name v = modify ((name, v) :) >> return True
    assert value match = return $ (value == match)

executeMany :: [Statement] -> ContextP Signal
executeMany [] = return SigReturn
executeMany (stmt : rest) = do
    sig <- execute stmt
    case sig of
        SigContinue -> executeMany rest
        _ -> return sig
