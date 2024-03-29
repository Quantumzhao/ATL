module Interpreter.Context
  ( Environment
  , ProgramState
  , getVars
  , putVars
  , getProcs
  , findVar
  , findVar'
  , findProc
  , doesExistVar
  , modifyVars
  , discardClosure )
  where

import Control.Monad.State ( StateT , gets , get , put , modify )
-- import Control.Monad.Except ( throwError )
import Types
import Data.List ( find )
import Data.Bifunctor ( first )

type KeyValuePair = (Variable, Value)
type Environment = ([KeyValuePair], [Procedure])
type ProgramState = StateT Environment Unchecked

-- getVar :: Environment -> (KeyValuePair -> Bool) -> Maybe Value
-- getVar ([], _) _ = Nothing
-- getVar (kvp@(_, value) : tl) f = do
--   if f kvp then return value
--   else find tl f

getVars :: ProgramState [KeyValuePair]
getVars = gets fst

putVars :: [KeyValuePair] -> ProgramState ()
putVars vars = do
  (_, ps) <- get
  put (vars, ps)

modifyVars :: ([KeyValuePair] -> [KeyValuePair]) -> ProgramState ()
modifyVars f = modify (first f)

getProcs :: ProgramState [Procedure]
getProcs = gets snd

findVar' :: (KeyValuePair -> Bool) -> ProgramState (Maybe KeyValuePair)
findVar' f = find f <$> getVars

findVar :: Variable -> ProgramState (Maybe Value)
findVar name = (snd <$>) <$> findVar' ((name ==) . fst)

findProc :: Variable -> ProgramState (Maybe Procedure)
findProc name = find (\(name', _, _) -> name == name') <$> getProcs

doesExistVar :: Variable -> ProgramState Bool
doesExistVar name = foldl (\a (var, _) -> (var == name) || a) False <$> getVars

discardClosure :: ProgramState a -> ProgramState a
discardClosure m = do
  env <- get
  v <- m
  put env
  return v