-- | This module contains an example of zippers for binary trees.
module Tree where

import Text.PrettyPrint hiding ( isEmpty )

-- -----------------------------------------------------------------------------
-- Binary tree

-- | Data type for binary trees.
data Tree a = Empty
            | Node a (Tree a) (Tree a)
  deriving Eq

-- | Functor instance for binary trees.
--
--   TODO: add doctest examples
--
instance Functor Tree where
  fmap f Empty        = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- | Foldable instance for binary trees.
--
--   TODO: add doctest examples
--
instance Foldable Tree where
  foldr _ y Empty        = y
  foldr f y (Node x r l) = f x (foldr f (foldr f y l) r)

-- | Alternative fold function.
--
--   TODO: add doctest examples
--
tfold :: (a -> b -> b -> b) -> b -> Tree a -> b
tfold _ y Empty        = y
tfold f y (Node x l r) = f x (tfold f y l) (tfold f y r)

-- | Smart constructor for leaves.
--
--   Examples
--
--   >>> leaf 1
--   (Node 1 Empty Empty)
--
--   >>> leaf True
--   (Node True Empty Empty)
--
leaf :: a -> Tree a
leaf x = Node x Empty Empty

-- | Constructs a perfect binary tree of a given height with all zeros.
--
--   Examples
--
--   >>> zeros 0
--   (Node 0 Empty Empty)
--
--   >>> zeros 1
--   (Node 0 (Node 0 Empty Empty)
--           (Node 0 Empty Empty))
--
zeros :: Int -> Tree Int
zeros n
  | n < 0  = error $ "negative height: " ++ show n
  | n == 0 = leaf 0
  | n > 0  = let t = zeros (n - 1) in Node 0 t t

-- -----------------------------------------------------------------------------
-- Pretty printing

-- | Show instance for binary trees.
instance Show a => Show (Tree a) where
  show = render . prettyTree

-- | Show instance for binary tree zipper contexts.
instance Show a => Show (Context a) where
  show = render . prettyContext

-- | Pretty print a tree.
prettyTree :: Show a => Tree a -> Doc
prettyTree Empty        = text "Empty"
prettyTree (Node a l r) = prettyNode "Node" isEmpty prettyTree prettyTree a l r

-- | Pretty print a zipper context.
prettyContext :: Show a => Context a -> Doc
prettyContext Hole      = text "Hole"
prettyContext (L a c r) = prettyNode "L " isHole prettyContext prettyTree a c r
prettyContext (R a l c) = prettyNode "R " isEmpty prettyTree prettyContext a l c

-- | Pretty print a zipper.
prettyZipper :: Show a => Zipper a -> Doc
prettyZipper (t, c) =
  text "Context:" $$ nest 2 (prettyContext c)
                  $$ text "Tree:"
                  $$ nest 2 (prettyTree t)

-- | Helper function for pretty printing.
prettyNode :: Show a =>
  String -> (b -> Bool) -> (b -> Doc) -> (c -> Doc) -> a -> b -> c -> Doc
prettyNode n e fl fr a l r =
  parens (text n <+> text v $$ nest indL (fl l) $$ nest indR (fr r))
  where
    v    = show a
    indL = 6 + length v
    indR = indL + if e l then 6 else 0

-- | Checks if a given tree is empty.
--
--   Examples
--
--   >>> isEmpty Empty
--   True
--
--   >>> isEmpty (leaf 1)
--   False
--
isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Checks if a given zipper context is a hole.
--
--   Examples
--
--   >>> isHole Hole
--   True
--
--   >>> isHole (L 1 Hole Empty)
--   False
--
isHole :: Context a -> Bool
isHole Hole = True
isHole _    = False

-- -----------------------------------------------------------------------------
-- Zipper

-- | Data type for binary tree zipper contexts.
data Context a = Hole
               | L a (Context a) (Tree a)
               | R a (Tree a) (Context a)
  deriving Eq

-- | Type synonym for binary tree zippers.
type Zipper a = (Tree a, Context a)

-- | Enter a zipper.
enter :: Tree a -> Zipper a
enter t = (t, Hole)

-- | Exit a zipper.
exit :: Zipper a -> Tree a
exit (t, Hole)     = t
exit (t, L x c r)  = exit (Node x t r, c)
exit (t, R x l c)  = exit (Node x l t, c)

-- | Focus on left subtree.
left :: Zipper a -> Zipper a
left (Node x l r, c) = (l, L x c r)
left (Empty, _)      = emptyError

-- | Focus on right subtree.
right :: Zipper a -> Zipper a
right (Node x l r, c) = (r, R x l c)
right (Empty, _)      = emptyError

-- | Focus on parent.
back :: Zipper a -> Zipper a
back (t, L x c r) = (Node x t r, c)
back (t, R x l c) = (Node x l t, c)
back (_, Hole)    = holeError

-- | Function composition with arguments flipped.
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

-- | Update the value at the focus of a zippers.
update :: (a -> a) -> Zipper a -> Zipper a
update f (Node x l r, z) = (Node (f x) l r, z)

-- | Error for attempting to focus on child when tree is empty.
emptyError :: a
emptyError = error "tree is empty"

-- | Error for attempting to focus on parent when context is hole.
holeError :: a
holeError = error "context is hole"

-- -----------------------------------------------------------------------------
-- Examples

-- | Examples of using zippers.
--
--   >>> t1
--   (Node 0 (Node 0 (Node 0 Empty Empty)
--                   (Node 0 Empty Empty))
--           (Node 0 (Node 0 Empty Empty)
--                   (Node 0 Empty Empty)))
--
--   >>> t2
--   (Node 0 (Node 1 (Node 0 Empty Empty)
--                   (Node 2 Empty Empty))
--           (Node 0 (Node 0 Empty Empty)
--                   (Node 0 Empty Empty)))
--
t1, t2 :: Tree Int
t1 = zeros 2
t2 = enter .> left .> update (+ 1) .> right .> update (+ 2) .> exit $ t1
