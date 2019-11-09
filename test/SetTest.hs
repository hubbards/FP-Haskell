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
tests = "Set" ~: [
  -- test isEmpty
    True  @=? isEmpty empty
  , False @=? isEmpty set1
  -- test insert
  , False @=? empty `contains` 1
  , True  @=? set1 `contains` 1
  , False @=? set1 `contains` 2
  , False @=? set2 `contains` 1
  , True  @=? set2 `contains` 2
  , False @=? set23 `contains` 1
  , True  @=? set23 `contains` 2
  , True  @=? set23 `contains` 3
  -- TODO: add union unit tests
  ] where
    set1  = empty `insert` 1
    set2  = empty `insert` 2
    set23 = set2 `insert` 3
