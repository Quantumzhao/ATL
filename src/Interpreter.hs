module Interpreter
(
    eval,
    evalM,
    execute,
    executeMany,
    -- run
)
where

import Types
import Assumptions( Environment , ContextP , doesExist , findByNameM )
import Control.Monad.State( get, modify , evalStateT )
import Control.Monad.Except( throwError )

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
evalM (LinkedListExpr ls) = do
    vs <- evalLL ls
    return $ LinkedList $ vs
    where
        evalLL [] = return []
        evalLL (hd : tl) = do
            v <- evalM hd
            vs <- evalLL tl
            return $ v : vs
evalM (StructExpr s) = do
    vs <- evalS s
    return $ Struct $ vs
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
execute (Switch expr ps) = do
    v <- evalM expr
    checkPatterns v ps
    where
    checkPattern v ptns = case (v, ptns) of
        (Int i, [Match mv]) -> assert (Int i) mv
        (Int i, [Bind i']) -> expand i' (Int i)
        (Bool b, [Match mv]) -> assert (Bool b) mv
        (Bool b, [Bind b']) -> expand b' (Bool b)
        (Unit, [Match mv]) -> assert Unit mv
        (Unit, [Bind u']) -> expand u' Unit
        _ -> return False
    checkPatterns _ [] = return SigContinue
    checkPatterns v ((ptn, body) : rest) = do
        b <- checkPattern v ptn
        if b then executeMany body
        else checkPatterns v rest 
    expand :: Variable -> Value -> ContextP Bool
    expand name v = modify ((name, v) :) >> return True
    assert value match = return $ value == match

executeMany :: [Statement] -> ContextP Signal
executeMany [] = return SigReturn
executeMany (stmt : rest) = do
    sig <- execute stmt
    case sig of
        SigContinue -> executeMany rest
        _ -> return sig
