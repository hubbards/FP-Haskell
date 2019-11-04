-- | This module contains an implementation of red-black trees.
--
-- A red-black tree is a balanced binary search tree (BST) where the following
-- invariants hold:
-- 1. each node is colored red or black
-- 2. the root is black
-- 3. if a node is red, then its children are black
-- 4. every path from a node to any empty descendant contains the same number of
--    black nodes
--
module RedBlack (
    Color (..)
  , Tree (Empty)
  , contains
  , insert
  ) where

-- | Data type for node color
data Color = Red
           | Black
  deriving (Eq, Show)

-- | Data type for red-black tree
data Tree a = Empty
            | Node Color a (Tree a) (Tree a)

-- | Checks if a tree contains a value.
--
-- NOTE: takes O(log n) time
--
contains :: Ord a => a -> Tree a -> Bool
contains _ Empty = False
contains x (Node _ y l r)
  | x < y     = contains x l
  | x > y     = contains x r
  | otherwise = True

-- | Inserts a value into a tree.
--
-- NOTE: takes O(log n) time
--
insert :: Ord a => a -> Tree a -> Tree a
insert x = setBlack . insert' x

setBlack :: Tree a -> Tree a
setBlack (Node _ x l r) = Node Black x l r
setBlack Empty          = undefined -- or error or absurd

insert' :: Ord a => a -> Tree a -> Tree a
insert' x Empty          = Node Red x Empty Empty
insert' x (Node c y l r) =
  if x <= y
    then checkL (Node c y (insert' x l) r)
    else checkR (Node c y l (insert' x r))

checkL :: Tree a -> Tree a
checkL (Node Black z (Node Red y (Node Red x a b) c) d) = balance x y z a b c d
checkL (Node Black z (Node Red x a (Node Red y b c)) d) = balance x y z a b c d
checkL t                                                = t

checkR :: Tree a -> Tree a
checkR (Node Black x a (Node Red y b (Node Red z c d))) = balance x y z a b c d
checkR (Node Black x a (Node Red z (Node Red y b c) d)) = balance x y z a b c d
checkR t                                                = t

balance :: a -> a -> a -> Tree a -> Tree a -> Tree a -> Tree a -> Tree a
balance x y z a b c d = Node Red y (Node Black x a b) (Node Black z c d)
