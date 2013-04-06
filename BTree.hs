{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.Monoid ((<>), mempty)
import qualified Data.Foldable as F
import qualified Data.Traversable as T

data Zero
data Succ s = Succ s

data Node s a where
    Leaf2 :: a -> Node Zero a
    Leaf3 :: a -> a -> Node Zero a
    Node2 :: Node s a -> a -> Node s a -> Node (Succ s) a
    Node3 :: Node s a -> a -> Node s a -> a -> Node s a -> Node (Succ s) a
instance F.Foldable (Node s) where
    foldMap f (Leaf2 x)         = f x
    foldMap f (Leaf3 x y)       = f x <> f y
    foldMap f (Node2 l x r)     = F.foldMap f l <> f x <> F.foldMap f r
    foldMap f (Node3 l x m y r) = F.foldMap f l <> f x <> F.foldMap f m <> f y <> F.foldMap f r

data BTree a where
    Root0 :: BTree a
    Root1 :: a -> BTree a
    RootN :: Node s a -> BTree a
instance F.Foldable BTree where
    foldMap _ Root0     = mempty
    foldMap f (Root1 x) = f x
    foldMap f (RootN n) = F.foldMap f n

empty :: BTree a
empty = Root0

member' :: forall a s . (Ord a) => a -> Node s a -> Bool
member' x = mb
  where
    mb :: forall s . Node s a -> Bool
    mb (Leaf2 y)        = y == x
    mb (Leaf3 y z)      = (y == x) || (z == x)
    mb (Node2 l y r)    = case compare x y of
        EQ  -> True
        LT  -> mb l
        GT  -> mb r
    mb (Node3 l y m z r)    = case compare x y of
        EQ  -> True
        LT  -> mb l
        GT  -> case compare x z of
            EQ  -> True
            LT  -> mb m
            GT  -> mb r

member :: Ord a => a -> BTree a -> Bool
member _ Root0      = False
member x (Root1 y)  = x == y
member x (RootN n)  = member' x n

insert' :: Ord a => a -> Node s a -> Either (Node s a, a, Node s a) (Node s a)
insert' x (Leaf2 y)     | x <= y    = Right $ Leaf3 x y
                        | otherwise = Right $ Leaf3 y x
insert' x (Leaf3 y z)
    | x < y     = Left (Leaf2 x, y, Leaf2 z)
    | x >= z    = Left (Leaf2 y, z, Leaf2 x)
    | otherwise = Left (Leaf2 y, x, Leaf2 z)
insert' x (Node2 l y r)
    | x < y     = case insert' x l of
        Right l'        -> Right $ Node2 l' y r
        Left (u, s, v)  -> Right $ Node3 u s v y r
    | otherwise = case insert' x r of
        Right r'        -> Right $ Node2 l y r'
        Left (u, s, v)  -> Right $ Node3 l y u s v
insert' x (Node3 l y m z r)
    | x < y     = case insert' x l of
        Right l'        -> Right $ Node3 l' y m z r
        Left (u, s, v)  -> Left $ (Node2 u s v, y, Node2 m z r)
    | x >= z    = case insert' x r of
        Right r'        -> Right $ Node3 l y m z r'
        Left (u, s, v)  -> Left $ (Node2 l y m, z, Node2 u s v)
    | otherwise = case insert' x m of
        Right m'        -> Right $ Node3 l y m' z r
        Left (u, s, v)  -> Left $ (Node2 l y u, s, Node2 v z r)

insert :: Ord a => a -> BTree a -> BTree a
insert x Root0      = Root1 x
insert x (Root1 y)
    | x <= y    = RootN (Leaf3 x y)
    | otherwise = RootN (Leaf3 y x)
insert x (RootN n)  = case insert' x n of
    Right n'        -> RootN n'
    Left (u, s, v)  -> RootN (Node2 u s v)
