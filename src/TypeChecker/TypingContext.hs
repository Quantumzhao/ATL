{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TypeChecker.TypingContext where

import Control.Monad.State ( StateT , get , put )
import Control.Monad.Except ( throwError )
import Types
import Prelude hiding ( id )
import Data.List ( find )

data Constraint = REqual VarExpr VarExpr
                | RApp Variable [VarExpr]
                | RAnd Constraint Constraint
                | ROr Constraint Constraint
                | RNot Constraint
type KeyValuePair = (Variable, TypeExpr)
type Context = [KeyValuePair]
type Ref'edVars = [VarExpr]
type DAG = ()
type ID = Int
-- P stands for partial
type EnvironmentP = StateT (Context, DAG, [Constraint], Ref'edVars, Int) Unchecked

genId :: EnvironmentP Int
genId = do
  (c, d, n, r, id) <- get
  let newId = id + 1
  put (c, d, n, r, newId)
  return newId

getContext :: EnvironmentP Context
getContext = do
  (context, _, _, _, _) <- get
  return context

getDAG :: EnvironmentP DAG
getDAG = do
  (_, dag, _, _, _) <- get
  return dag

getRef'edVars :: EnvironmentP Ref'edVars
getRef'edVars = do
  (_, _, _, vs, _) <- get
  return vs

getConstraintsOn :: VarExpr -> EnvironmentP Constraint
getConstraintsOn = undefined

removeConstraintsOn :: VarExpr -> EnvironmentP Constraint
removeConstraintsOn = undefined

addConstraint :: Constraint -> EnvironmentP ()
addConstraint = undefined

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

-- find :: Context -> (KeyValuePair -> Bool) -> Maybe TypeExpr
-- find [] _ = Nothing
-- find (kvp@(_, value) : tl) f = do
--   if f kvp then return value
--   else find tl f

-- findM :: (KeyValuePair -> Bool) -> EnvironmentP TypeExpr
-- findM f = do
--   env <- getContext
--   case find env f of
--     Just v -> return v
--     Nothing -> throwError "not found"

findByNameM :: Variable -> EnvironmentP TypeExpr
findByNameM name = do
  env <- getContext
  case find ((name ==) . fst) env of
    Just v -> return $ snd v
    Nothing -> throwError "not found"

findLub :: [ID] -> EnvironmentP ID
findLub = undefined

findGlb :: [ID] -> EnvironmentP ID
findGlb = undefined

intersect :: [ID] -> EnvironmentP ID
intersect = undefined

union :: [ID] -> EnvironmentP ID
union = undefined

strictlyGt :: ID -> ID -> EnvironmentP Bool
strictlyGt = undefined

doesExist :: Context -> Variable -> Bool
doesExist env name = foldl (\a (var, _) -> (var == name) || a) False env

tryFindID :: Variable -> EnvironmentP (Maybe ID)
tryFindID = undefined

tryMoveBelow :: ID -> ID -> EnvironmentP Bool
tryMoveBelow = undefined

tryMoveAbove :: ID -> ID -> EnvironmentP (Maybe ID)
tryMoveAbove = undefined

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

findTypeID :: Variable -> EnvironmentP ID
findTypeID = undefined

setVarToTypeID :: Variable -> ID -> EnvironmentP ()
setVarToTypeID = undefined
