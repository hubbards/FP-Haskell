-- | This module contains an example of zippers for binary trees.
module Tree (
    Tree (..)
  , tfoldr
  , leaf
  , zeros
  , isEmpty

  , Zipper
  , Context (..)
  , isHole
  , enter
  , exit
  , left
  , right
  , back
  , (.>)
  , update
  , prettyZipper
  ) where

-- NOTE: from pretty package
import Text.PrettyPrint hiding ( isEmpty )

-- -----------------------------------------------------------------------------
-- Binary tree data type and type class instances

-- | Data type for binary trees.
data Tree a = Empty
            | Node a (Tree a) (Tree a)
  deriving Eq

-- | Functor instance for binary trees.
instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- | Foldable instance for binary trees.
instance Foldable Tree where
  foldr _ y Empty        = y
  foldr f y (Node x r l) = f x (foldr f (foldr f y l) r)

-- -----------------------------------------------------------------------------
-- Binary tree functions

-- | Alternative fold function.
tfoldr :: (a -> b -> b -> b) -> b -> Tree a -> b
tfoldr _ y Empty        = y
tfoldr f y (Node x l r) = f x (tfoldr f y l) (tfoldr f y r)

-- | Smart constructor for leaves.
leaf :: a -> Tree a
leaf x = Node x Empty Empty

-- | Constructs a perfect binary tree of a given height with all zeros.
zeros :: Int -> Tree Int
zeros n
  | n < 0     = error $ "negative height: " ++ show n
  | n == 0    = leaf 0
  | otherwise = let t = zeros (n - 1) in Node 0 t t

-- | Checks if a given tree is empty.
isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- -----------------------------------------------------------------------------
-- Binary tree zipper

-- | Type synonym for binary tree zippers.
--
-- Examples
--
-- >>> enter .> left .> update (+ 1) .> right .> update (+ 2) .> exit $ zeros 2
-- (Node 0 (Node 1 (Node 0 Empty Empty)
--                 (Node 2 Empty Empty))
--         (Node 0 (Node 0 Empty Empty)
--                 (Node 0 Empty Empty)))
--
type Zipper a = (Tree a, Context a)

-- | Data type for binary tree zipper contexts.
data Context a = Hole
               | L a (Context a) (Tree a)
               | R a (Tree a) (Context a)
  deriving Eq

-- | Checks if a given zipper context is a hole.
isHole :: Context a -> Bool
isHole Hole = True
isHole _    = False

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
update f (Node x l r, c) = (Node (f x) l r, c)
update _ z               = z

-- | Error for attempting to focus on child when tree is empty.
emptyError :: a
emptyError = error "tree is empty"

-- | Error for attempting to focus on parent when context is hole.
holeError :: a
holeError = error "context is hole"

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
prettyNode ::
  Show a =>
  String -> (b -> Bool) -> (b -> Doc) -> (c -> Doc) -> a -> b -> c -> Doc
prettyNode n e fl fr a l r =
  parens (text n <+> text v $$ nest indL (fl l) $$ nest indR (fr r))
  where
    v    = show a
    indL = 6 + length v
    indR = indL + if e l then 6 else 0
