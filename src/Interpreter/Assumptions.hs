module Interpreter.Assumptions
    (
        Environment,
        ContextP,
        find,
        findM,
        findByNameM,
        doesExist
    )
    where

import Control.Monad.State ( StateT , get )
import Control.Monad.Except ( throwError )
import Interpreter.Types

type KeyValuePair = (Variable, Value)
type Environment = [KeyValuePair]
-- P stands for partial
type ContextP = StateT Environment Unchecked

find :: Environment -> (KeyValuePair -> Bool) -> Maybe Value
find [] _ = Nothing
find (kvp@(_, value) : tl) f = do
    if f kvp then return value
    else find tl f

findM :: (KeyValuePair -> Bool) -> ContextP Value
findM f = do
    env <- get
    case find env f of
        Just v -> return v
        Nothing -> throwError "not found"

findByNameM :: Variable -> ContextP Value
findByNameM name = findM $ \x -> fst x == name

doesExist :: Environment -> Variable -> Bool
doesExist env name = foldl (\a (var, _) -> (var == name) || a) False env
