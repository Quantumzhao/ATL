{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module TypeChecker.TypingContext where

import Control.Monad.State ( StateT , get , put , modify )
import Control.Monad.Except ( throwError )
import Types
import Prelude hiding ( id )
import Data.List ( find )
import Control.Applicative (Applicative(liftA2))
import Control.Monad.Extra (findM, allM)
import Control.Monad (filterM)
import Data.Functor.Contravariant (Equivalence)
import qualified TypeChecker.Graph ( Vertex(V) , Graph )
import TypeChecker.Graph
  ( elem'
  , insert'
  , filter'
  , Graph ( G , getVertices , getEdges )
  , single, Vertex (getValue) )

data Constraint = REqual VarExpr VarExpr
                | RApp Variable [VarExpr]
                | RAnd Constraint Constraint
                | ROr Constraint Constraint
                | RNot Constraint
type KeyValuePair = (Variable, ID)
type Context = [KeyValuePair]
type ID = Int
type VertexID = Int
type Vertex = TypeChecker.Graph.Vertex ID TypeExpr
type Dag = TypeChecker.Graph.Graph ID TypeExpr
-- P stands for partial
type EnvironmentP = StateT (Context, Dag, [Constraint], Int) Unchecked

genId :: EnvironmentP Int
genId = do
  (c, d, n, id) <- get
  let newId = id + 1
  put (c, d, n, newId)
  return newId

getContext :: EnvironmentP Context
getContext = do
  (context, _, _, _) <- get
  return context

getDag :: EnvironmentP Dag
getDag = do
  (_, g, _, _) <- get
  return g

isDependentOn :: Constraint -> VarExpr -> Bool
isDependentOn (REqual v1 v2) v' = v1 == v' && v2 == v'
isDependentOn (RApp _ ps) v' = v' `elem` ps
isDependentOn (RAnd c1 c2) v = c1 `isDependentOn` v || c2 `isDependentOn` v
isDependentOn (ROr c1 c2) v = c1 `isDependentOn` v || c2 `isDependentOn` v
isDependentOn (RNot c) v = c `isDependentOn` v

getConstraintsOn :: VarExpr -> EnvironmentP [Constraint]
getConstraintsOn v = do
  (_, _, cs, _) <- get
  return $ filter (`isDependentOn` v) cs

removeConstraintsOn :: VarExpr -> EnvironmentP ()
removeConstraintsOn v = do
  (cn, d, cs, i) <- get
  let cs' = foldl (\a x -> if x `isDependentOn` v then a else x : a) [] cs'
  put (cn, d, cs', i)

removeConstraintsOn' :: [VarExpr] -> EnvironmentP ()
removeConstraintsOn' = foldl (const removeConstraintsOn) $ pure ()

addConstraint :: Constraint -> EnvironmentP ()
addConstraint c = modify $ \(cn, d, cs, i) -> (cn, d, c : cs, i)

-- second ID encodes the subgraph
-- findSuprema :: TypeExpr -> ID -> EnvironmentP [ID]
-- findSuprema t_expr id = do
--   ns <- parents id
--   foldl (\a x -> do
--     t_x <- findByID x
--     t_x `strictlyGt` t_expr >>= \case
--       Just True -> (x :) <$> a
--       Just False -> (<>) <$> findInfima t_expr x <*> a
--       Nothing -> a
--     ) (pure []) ns
findInfima :: TypeExpr -> ID -> EnvironmentP [ID]
findInfima t_expr id = do undefined
--   ns <- parents id
--   foldl (\a x -> do
--     m_x <- findByID x
--     case m_x of
--       Just t_x -> t_x `strictlyGt` t_expr >>= \case
--         Just True -> (x :) <$> a
--         Just False -> (<>) <$> findInfima t_expr x <*> a
--         Nothing -> a
--       Nothing -> (<>) <$> findInfima t_expr x <*> a
--     ) (pure []) ns

strictlyGt :: TypeExpr -> TypeExpr -> EnvironmentP (Maybe Bool)
strictlyGt = undefined

appendVertex :: ID -> Maybe TypeExpr -> EnvironmentP VertexID
appendVertex id t_expr = do
  newId <- genId
  modify (\(cn, G vs es, cs, i) -> (cn, G ((TypeChecker.Graph.V (single id) t_expr newId) : vs) es, cs, i))
  return newId

appendEdge :: VertexID -> VertexID -> EnvironmentP ()
appendEdge id1 id2 = do
  modify (\(cn, G vs es, cs, i) ->
    (cn, G vs ((id1, id2) : es), cs, i))

-- this forms quotient classes from equivalence relation
equals :: TypeExpr -> TypeExpr -> EnvironmentP Bool
equals TUnit TUnit = return True
equals TTop TTop = return True
equals TBottom TBottom = return True
equals (TBool (BValue True)) (TBool (BValue True)) = return True
equals (TBool (BValue False)) (TBool (BValue False)) = return True
equals (TInt IInteger) (TInt IInteger) = return True
equals (TInt (INumber i1)) (TInt (INumber i2)) = return $ i1 == i2
equals (TMap ps1 ret1 vars1) (TMap ps2 ret2 vars2) = undefined
equals _ _ = return False

-- if type is a numeric type, find the maximum member
-- getMax :: TypeInt -> Int
-- getMax = undefined

-- getMin :: TypeInt -> Int
-- getMin = undefined

-- findByNameM :: Variable -> EnvironmentP TypeExpr
-- findByNameM name = do
--   env <- getContext
--   case find ((name ==) . fst) env of
--     Just v -> return $ snd v
--     Nothing -> throwError "not found"

-- findLub :: [ID] -> EnvironmentP ID
-- findLub = undefined

-- findGlb :: [ID] -> EnvironmentP ID
-- findGlb = undefined

intersect :: [ID] -> EnvironmentP ID
intersect = undefined

-- union :: [ID] -> EnvironmentP ID
-- union [] = return bottomType
-- union ids@(hd : tl) = do
--   allM (<=< intType) ids >>= \b -> if b then
--     return $ reduce
--   else
--     undefined
--   where
--     reduce = undefined

parents :: VertexID -> EnvironmentP [VertexID]
parents vertex = do
  (snd <$>) . (filter ((vertex ==) . snd) . getEdges) <$> getDag

(>=>), (<=<), (<-<) :: VertexID -> VertexID -> EnvironmentP Bool
id_1 >=> id_2 = do
  g <- getDag
  foldl (\a x -> (||) <$> a <*> x >=> id_2) (pure False) =<< parents id_2
id_1 <=< id_2 = do
  g <- getDag
  foldl (\a x -> (||) <$> a <*> x <=< id_2) (pure False) =<< parents id_1
id_1 <-< id_2 = liftA2 (&&) (id_1 <=< id_2) (pure $ id_1 /= id_2)

doesExist :: Context -> Variable -> Bool
doesExist env name = foldl (\a -> (a ||) . (name ==) . fst) False env

tryFindID :: Variable -> EnvironmentP (Maybe ID)
tryFindID var = do
  (snd <$>) . find ((var ==) . fst) <$> getContext

tryMoveBelow :: ID -> ID -> EnvironmentP Bool
tryMoveBelow = undefined

tryMoveAbove :: ID -> ID -> EnvironmentP (Maybe ID)
tryMoveAbove = undefined

genUnknownType :: EnvironmentP VertexID
genUnknownType = do
  newId <- genId
  vId <- appendVertex newId Nothing
  appendEdge newId topType
  return vId

-- if type is unique, i.e. ∀ t' ∈ G, t' != t, success and return a new ID
-- otherwise returns the existing type ID
tryUpdateG :: TypeExpr -> EnvironmentP ID
tryUpdateG t_expr = do
  g <- getDag
  findM (equals t_expr . getValue) (getVertices g) >>= \case
    Nothing -> do
      newId <- genId
      infs <- findInfima t_expr bottomType
      _ <- appendVertex newId (Just t_expr)
      foldl (\_ x -> appendEdge newId x) (pure ()) infs
      return newId
    Just (TypeChecker.Graph.V s t id) -> return id

tryUpdateGMany :: [TypeExpr] -> EnvironmentP [ID]
tryUpdateGMany [] = return []
tryUpdateGMany (x : xs) = do
  x' <- tryUpdateG x
  xs' <- tryUpdateGMany xs
  return (x' : xs')

findByID :: ID -> EnvironmentP (Maybe TypeExpr)
findByID id = do
  kvps <- getVertices <$> getDag
  case filter (\(TypeChecker.Graph.V s _ _) -> id `elem'` s) kvps of
    [] -> throwError "findByID: ID not found"
    (TypeChecker.Graph.V _ t _) : _ -> return t

findTypeID :: Variable -> EnvironmentP ID
findTypeID var = do
  c <- getContext
  case filter ((== var) . fst) c of
    [] -> throwError "ID not found"
    (_, t) : _ -> return t

setVarToTypeID :: Variable -> ID -> EnvironmentP ()
setVarToTypeID var id = modify (\(cn, d, cs, i) -> (setVarToTypeID' cn, d, cs, i))
  where
    setVarToTypeID' = foldl (\a (k, v) -> if k == var then (k, id) : a else (k, v) : a) []

eqType :: ID
eqType = 1

gtType :: ID
gtType = 2

ltType :: ID
ltType = 3

andType :: ID
andType = 4

orType :: ID
orType = 5

notType :: ID
notType = 6

addType :: ID
addType = 7

subtractType :: ID
subtractType = 8

intType :: ID
intType = 9

boolType :: ID
boolType = 12

unitType :: ID
unitType = 13

trueType :: ID
trueType = 14

falseType :: ID
falseType = 15

bottomType :: ID
bottomType = 10

topType :: ID
topType = 11
