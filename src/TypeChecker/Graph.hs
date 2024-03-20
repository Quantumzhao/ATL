{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE InstanceSigs #-}

module TypeChecker.Graph where

data Eq t => NonEmptySet t = S t [t]
instance Eq t => Eq (NonEmptySet t) where
  (==) :: NonEmptySet t -> NonEmptySet t -> Bool
  (S a as) == (S b bs) =
    a == b && length as == length bs && foldl (\a' x -> a' && x `elem` bs) True bs

data Eq t => Vertex t o = V { getSet :: NonEmptySet t
                            , getValue :: o
                            , getID :: Int }
instance Eq t => Eq (Vertex t o) where
  (V _ _ id1) == (V _ _ id2) = id1 == id2

data Eq t => Graph t o = G { getVertices :: [Vertex t o]
               , getEdges :: [(t, t)] }

filter' :: Eq a => (a -> Bool) -> NonEmptySet a -> [a]
filter' f (S a as) = if f a then a : filter f as else filter f as

elem' :: Eq a => a -> NonEmptySet a -> Bool
elem' a (S a' as) = a == a' || a `elem` as

insert' :: Eq a => a -> NonEmptySet a -> NonEmptySet a
insert' a n@(S b bs)
  | a == b = n
  | a `elem` bs = n
  | otherwise = S a $  b : bs

single :: Eq a => a -> NonEmptySet a
single a = S a []


