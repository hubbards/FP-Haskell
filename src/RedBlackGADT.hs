{-# LANGUAGE GADTs #-}

-- | This module contains an implementation of a red-black tree using phantom
-- types and GADT to encode invariants in types.
module RedBlackGADT (
    Red
  , Black
  , Tree (Empty)
  , insert
  , contains
  ) where

-- | Phantom type for red
data Red

-- | Phantom type for black
data Black

-- | GADT for red-black tree
data Tree c a where
  Empty :: Tree Black a
  NodeB :: a -> Tree l a -> Tree r a -> Tree Black a
  -- NOTE: invariant #3 is encoded in the type
  NodeR :: a -> Tree Black a -> Tree Black a -> Tree Red a

-- | Checks if a tree contains a value.
--
-- NOTE: takes O(log n) time
--
contains :: Ord a => a -> Tree c a -> Bool
contains x Empty         = False
contains x (NodeB y l r) = contains' x y l r
contains x (NodeR y l r) = contains' x y l r

contains' :: Ord a => a -> a -> Tree l a -> Tree r a -> Bool
contains' x y l r
  | x < y     = contains x l
  | x > y     = contains x r
  | otherwise = True

-- | Inserts a value into a tree.
--
-- NOTE: takes O(log n) time
--
insert :: Ord a => a -> Tree Black a -> Tree Black a
insert x t = toTreeB (insert' x t)

insert' :: Ord a => a -> Tree c a -> Temp a
insert' x Empty = TempR x Empty Empty
insert' x (NodeB y l r) =
  if x <= y
    then checkL y (insert' x l) r
    else checkR y l (insert' x r)
insert' x (NodeR y l r) =
  if x <= y
    then TempR y (toTreeR $ insert' x l) r
    else TempR y l (toTreeR $ insert' x r)

-- temporary node, allows invariant violations
data Temp a where
  TempB :: a -> Tree l a -> Tree r a -> Temp a
  TempR :: a -> Tree l a -> Tree r a -> Temp a

toTreeB :: Temp a -> Tree Black a
toTreeB (TempB x l r) = NodeB x l r
toTreeB (TempR x l r) = NodeB x l r

toTreeR :: Temp a -> Tree Red a
toTreeR (TempR x Empty Empty)                     = NodeR x Empty Empty
toTreeR (TempR x Empty r@(NodeB _ _ _))           = NodeR x Empty r
toTreeR (TempR x l@(NodeB _ _ _) Empty)           = NodeR x l Empty
toTreeR (TempR x l@(NodeB _ _ _) r@(NodeB _ _ _)) = NodeR x l r

checkL :: a -> Temp a -> Tree c a -> Temp a
checkL z (TempR y (NodeR x a b) c) d                 = balance x y z a b c d
checkL z (TempR x a (NodeR y b c)) d                 = balance x y z a b c d
-- non-interesting cases
checkL x (TempR y Empty Empty) t                     = TempB x (NodeR y Empty Empty) t
checkL x (TempR y l@(NodeB _ _ _) Empty) t           = TempB x (NodeR y l Empty) t
checkL x (TempR y Empty r@(NodeB _ _ _)) t           = TempB x (NodeR y Empty r) t
checkL x (TempR y l@(NodeB _ _ _) r@(NodeB _ _ _)) t = TempB x (NodeR y l r) t
checkL x (TempB y l r) t                             = TempB x (NodeB y l r) t

checkR :: a -> Tree c a -> Temp a -> Temp a
checkR x a (TempR y b (NodeR z c d))                 = balance x y z a b c d
checkR x a (TempR z (NodeR y b c) d)                 = balance x y z a b c d
-- non-interesting cases
checkR x t (TempR y Empty Empty)                     = TempB x t (NodeR y Empty Empty)
checkR x t (TempR y l@(NodeB _ _ _) Empty)           = TempB x t (NodeR y l Empty)
checkR x t (TempR y Empty r@(NodeB _ _ _))           = TempB x t (NodeR y Empty r)
checkR x t (TempR y l@(NodeB _ _ _) r@(NodeB _ _ _)) = TempB x t (NodeR y l r)
checkR x t (TempB y l r)                             = TempB x t (NodeB y l r)

balance :: a -> a -> a -> Tree l1 a -> Tree r1 a -> Tree l2 a -> Tree r2 a -> Temp a
balance x y z a b c d = TempR y (NodeB x a b) (NodeB z c d)
