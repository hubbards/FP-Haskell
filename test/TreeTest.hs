module TreeTest ( tests ) where

import Test.HUnit (
    Test
  , Assertion
  , (~:)
  , (@=?)
  , (@?)
  , test
  )
import Tree

tests :: [Test]
tests =
  [ "basic operations" ~:
    [ "leaf" ~:
      [ Node 1 Empty Empty    @=? leaf 1
      , Node True Empty Empty @=? leaf True ]
    , "zeros" ~:
      [ leaf 0                   @=? zeros 0
      , Node 0 (leaf 0) (leaf 0) @=? zeros 1 ]
    , "isEmpty" ~:
      [ isEmpty Empty          @? "Empty"
      , not (isEmpty $ leaf 1) @? "Non-empty" ] ]
  , "zipper operations" ~:
    [ "isHole" ~:
      [ isHole Hole                   @? "Hole"
      , not (isHole $ L 1 Hole Empty) @? "Non-hole" ]
    -- TODO: more zipper unit tests
    ] ]
