module Assumptions
    (
        Environment,
        ContextP,
        find
    )
    where

import Control.Monad.State (StateT)
import Control.Monad.Except (MonadError(throwError))
import Types

type Environment = [(Variable, Value)]
type ContextP = StateT Environment Unchecked

find :: Environment -> Variable -> Unchecked Value
find [] name = throwError $ "Environment does not contain" <> name
find ((key, value) : tl) name = 
    if key == name then return value
    else find tl name


