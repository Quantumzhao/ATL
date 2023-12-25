module Evaluator 
    (

    )
    where

import Types
    (
        Expr(..),
        Value(..),
        Error,
    )

eval :: Expr -> Either Error Value
eval (Literal l) = Right l
eval (Add e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (Int i1, Int i2) -> return $ Int $ i1 + i2
        (_, _) -> Left "type error: Add"
eval (Subtract e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (Int i1, Int i2) -> return $ Int $ i1 - i2
        (_, _) -> Left "type error: Subtract"
eval (Not e) = do
    v <- eval e
    case v of
        (Bool b) -> return $ Bool $ not b
        _ -> Left "type error: Not"
eval (Or e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (Bool b1, Bool b2) -> return $ Bool $ b1 || b2
        (_, _) -> Left "type error: Or"
eval (And e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (Bool b1, Bool b2) -> return $ Bool $ b1 && b2
        (_, _) -> Left "type error: And"
eval (Equal e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    return $ Bool $ v1 == v2
eval (GreaterThan e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (Int i1, Int i2) -> return $ Bool $ i1 > i2
        (_, _) -> Left "type error: GreaterThan"
eval (LessThan e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (Int i1, Int i2) -> return $ Bool $ i1 > i2
        (_, _) -> Left "type error: GreaterThan"
eval (ArrayExpr as) = do
    vs <- evalArr as
    return $ Array $ vs
    where
        evalArr [] = Right []
        evalArr (hd : tl) = do
            v <- eval hd
            vs <- evalArr tl
            return $ v : vs
eval (LinkedListExpr ls) = do
    vs <- evalLL ls
    return $ LinkedList $ vs
    where
        evalLL [] = Right []
        evalLL (hd : tl) = do
            v <- eval hd
            vs <- evalLL tl
            return $ v : vs
eval (StructExpr s) = do
    vs <- evalS s
    return $ Struct $ vs
    where
        evalS [] = Right []
        evalS ((name, hd) : tl) = do
            v <- eval hd
            vs <- evalS tl
            return $ (name, v) : vs
