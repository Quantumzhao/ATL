{-# LANGUAGE FlexibleContexts #-}
module Evaluator 
    (
        eval
    )
    where

import Types
    (
        Expr(..),
        Value(..),
        Unchecked
    )
import Assumptions (ContextP, Environment, find)
import Control.Monad.Except (throwError)
import Control.Monad.State (evalStateT, get, lift)

eval :: Environment -> Expr -> Unchecked Value
eval env expr = evalStateT (eval' expr) env

eval' :: Expr -> ContextP Value
eval' (Literal l) = return l
eval' (Add e1 e2) = do
    v1 <- eval' e1
    v2 <- eval' e2
    case (v1, v2) of
        (Int i1, Int i2) -> return $ Int $ i1 + i2
        (_, _) -> throwError "type error: Add"
eval' (Subtract e1 e2) = do
    v1 <- eval' e1
    v2 <- eval' e2
    case (v1, v2) of
        (Int i1, Int i2) -> return $ Int $ i1 - i2
        (_, _) -> throwError "type error: Subtract"
eval' (Not e) = do
    v <- eval' e
    case v of
        (Bool b) -> return $ Bool $ not b
        _ -> throwError "type error: Not"
eval' (Or e1 e2) = do
    v1 <- eval' e1
    v2 <- eval' e2
    case (v1, v2) of
        (Bool b1, Bool b2) -> return $ Bool $ b1 || b2
        (_, _) -> throwError "type error: Or"
eval' (And e1 e2) = do
    v1 <- eval' e1
    v2 <- eval' e2
    case (v1, v2) of
        (Bool b1, Bool b2) -> return $ Bool $ b1 && b2
        (_, _) -> throwError "type error: And"
eval' (Equal e1 e2) = do
    v1 <- eval' e1
    v2 <- eval' e2
    return $ Bool $ v1 == v2
eval' (GreaterThan e1 e2) = do
    v1 <- eval' e1
    v2 <- eval' e2
    case (v1, v2) of
        (Int i1, Int i2) -> return $ Bool $ i1 > i2
        (_, _) -> throwError "type error: GreaterThan"
eval' (LessThan e1 e2) = do
    v1 <- eval' e1
    v2 <- eval' e2
    case (v1, v2) of
        (Int i1, Int i2) -> return $ Bool $ i1 > i2
        (_, _) -> throwError "type error: GreaterThan"
eval' (ArrayExpr as) = do
    vs <- evalArr as
    return $ Array $ vs
    where
        evalArr [] = return []
        evalArr (hd : tl) = do
            v <- eval' hd
            vs <- evalArr tl
            return $ v : vs
eval' (LinkedListExpr ls) = do
    vs <- evalLL ls
    return $ LinkedList $ vs
    where
        evalLL [] = return []
        evalLL (hd : tl) = do
            v <- eval' hd
            vs <- evalLL tl
            return $ v : vs
eval' (StructExpr s) = do
    vs <- evalS s
    return $ Struct $ vs
    where
        evalS [] = return []
        evalS ((name, hd) : tl) = do
            v <- eval' hd
            vs <- evalS tl
            return $ (name, v) : vs
eval' (Variable var) = do
    env <- get
    value <- lift $ find env var
    return value
