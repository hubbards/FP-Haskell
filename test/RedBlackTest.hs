module RedBlackTest ( tests ) where

-- NOTE: from HUnit package
import Test.HUnit (
    Test
  , (~:)
  , (~?=)
  , (@=?)
  , (@?)
  )

import RedBlack

tests :: Test
tests = "RedBlack" ~:
  [ contains 1 Empty  @=? False
  , contains 1 tree1  @=? True
  , contains 1 tree2  @=? False
  , contains 2 tree21 @=? True
  , contains 2 tree12 @=? True
  -- TODO: add more unit tests
  ]
  where
    tree1  = insert 1 Empty
    tree2  = insert 2 Empty
    tree21 = insert 2 tree1
    tree12 = insert 1 tree2
