module Interpreter
(
    -- execute,
    -- run
)
where

import Evaluator
import Types
import Assumptions

-- execute :: Statement -> Environment -> Either Error Environment
-- execute (Assign var expr) env = do
--     v <- eval expr
--     return $ (var, v) : env
-- execute _ _ = error ""

-- run :: Program -> Either Error ()
-- run [] = Right ()
-- run (stmt : rst) = error ""

