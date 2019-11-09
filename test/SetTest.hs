module SetTest ( tests ) where

-- NOTE: from HUnit package
import Test.HUnit (
    Test
  , (~:)
  , (~?=)
  , (@=?)
  , (@?)
  )

import Set

tests :: Test
tests = "Set" ~:
  [ "empty" ~:
    [ isEmpty empty      @=? True
    , empty `contains` 1 @=? False ]
  , "insert" ~:
    [ set1 `contains` 1  @=? True
    , set1 `contains` 2  @=? False
    , set2 `contains` 1  @=? False
    , set2 `contains` 2  @=? True
    , set12 `contains` 1 @=? True
    , set12 `contains` 2 @=? True ]
  -- TODO: add union tests
  ] where
    set1  = empty `insert` 1
    set2  = empty `insert` 2
    set12 = set1 `insert` 2
    set23 = set2 `insert` 3
