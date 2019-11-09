module TreeTest ( tests ) where

-- NOTE: from HUnit package
import Test.HUnit (
    Test
  , (~:)
  , (@=?)
  , (@?)
  )

import Tree

tests :: Test
tests = "Tree" ~: [
    testFunctor
  , testFoldable
  -- TODO: add more unit tests
  ]


testFunctor :: Test
testFunctor = "Functor" ~: [
    Empty                    @=? fmap (+ 1) Empty
  , Node 2 Empty Empty       @=? fmap (+ 1) (leaf 1)
  , Node 2 (leaf 3) Empty    @=? fmap (+ 1) (Node 1 (leaf 2) Empty)
  , Node 2 Empty (leaf 3)    @=? fmap (+ 1) (Node 1 Empty (leaf 2))
  , Node 2 (leaf 3) (leaf 4) @=? fmap (+ 1) (Node 1 (leaf 2) (leaf 3))
  ]

testFoldable :: Test
testFoldable = "Foldable" ~: [
    0 @=? foldr 0 (+) Empty
  , 1 @=? foldr 0 (+) (leaf 1)
  , 3 @=? foldr 0 (+) (Node 1 (leaf 2) Empty)
  , 3 @=? foldr 0 (+) (Node 1 Empty (leaf 2))
  , 6 @=? foldr 0 (+) (Node 1 (leaf 2) (leaf 3))
  ]
