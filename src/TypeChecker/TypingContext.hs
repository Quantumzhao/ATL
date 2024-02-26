module TypeChecker.TypingContext
  ( Context
  , EnvironmentP
  , ID
  , findByID
  , findM
  , findByNameM
  , findType
  , findLub
  , doesExist
  , equals
  , genId
  , getMax
  , getMin
  , getDAG
  , getContext
  , isConnected
  , tryUpdateDAG
  , tryUpdateDAGMany
  , strictlyGt )
  where

import Control.Monad.State ( StateT , get , put )
import Control.Monad.Except ( throwError )
import Types
import Prelude hiding ( id )

type KeyValuePair = (Variable, TypeExpr)
type Context = [KeyValuePair]
type DAG = ()
type ID = Int
-- P stands for partial
type EnvironmentP = StateT (Context, DAG, ID) Unchecked

genId :: EnvironmentP Int
genId = do
  (c, d, id) <- get
  let newId = id + 1
  put (c, d, newId)
  return newId

getContext :: EnvironmentP Context
getContext = do
  (context, _, _) <- get
  return context

getDAG :: EnvironmentP DAG
getDAG = do
  (_, dag, _) <- get
  return dag

findSup :: ID -> EnvironmentP KeyValuePair
findSup = undefined

findInf :: ID -> EnvironmentP KeyValuePair
findInf = undefined

isConnected :: ID -> ID -> DAG -> Bool
isConnected = undefined

equals :: TypeExpr -> TypeExpr -> Bool
equals = undefined

-- if type is a numeric type, find the maximum member
getMax :: TypeExpr -> Int
getMax = undefined

getMin :: TypeExpr -> Int
getMin = undefined

find :: Context -> (KeyValuePair -> Bool) -> Maybe TypeExpr
find [] _ = Nothing
find (kvp@(_, value) : tl) f = do
  if f kvp then return value
  else find tl f

findM :: (KeyValuePair -> Bool) -> EnvironmentP TypeExpr
findM f = do
  env <- getContext
  case find env f of
    Just v -> return v
    Nothing -> throwError "not found"

findByNameM :: Variable -> EnvironmentP TypeExpr
findByNameM name = findM $ \x -> fst x == name

findLub :: [ID] -> EnvironmentP ID
findLub = undefined

strictlyGt :: ID -> ID -> EnvironmentP Bool
strictlyGt = undefined

doesExist :: Context -> Variable -> Bool
doesExist env name = foldl (\a (var, _) -> (var == name) || a) False env

findType :: Variable -> EnvironmentP ID
findType = undefined

-- if type is unique, i.e. ∀ t' ∈ G, t' != t, success and return a new ID
-- otherwise returns the existing type ID
tryUpdateDAG :: TypeExpr -> EnvironmentP ID
tryUpdateDAG = undefined

tryUpdateDAGMany :: [TypeExpr] -> EnvironmentP [ID]
tryUpdateDAGMany [] = return []
tryUpdateDAGMany (x : xs) = do
  x' <- tryUpdateDAG x
  xs' <- tryUpdateDAGMany xs
  return (x' : xs')


findByID :: ID -> EnvironmentP TypeExpr
findByID id = undefined
