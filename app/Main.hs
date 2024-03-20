module Main (main) where

import Interpreter.Core ( execWithEnv )
import Parser ( parse , parseProgram )
import Types

main :: IO ()
main = do
  src <- readFile "test.txt"
  case parse src of
    Right ast -> case execWithEnv ([], []) ast of
      Right (sig, env) -> case sig of
        SigReturn v -> print v
        _ -> print "pass"
      Left msg -> print msg
    Left msg -> print msg
