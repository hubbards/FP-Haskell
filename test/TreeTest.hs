module TreeTest ( tests ) where

import Test.HUnit (
    Test
  , Assertion
  , (~:)
  , (@=?)
  , test
  )
import Tree

tests :: [Test]
tests =
  [ "basic functions" ~:
    [ "leaf" ~:
      [ Node 1 Empty Empty @=? leaf 1
      , Node True Empty Empty @=? leaf True ]
    , "zeros" ~:
      [ leaf 0 @=? zeros 0
      , Node 0 (leaf 0) (leaf 0) @=? zeros 1 ]
    , "isEmpty" ~:
      [ True  @=? isEmpty Empty
      , False @=? isEmpty (leaf 1) ] ]
  , "zipper functions" ~:
    [ "isHole" ~:
      [ True  @=? isHole Hole
      , False @=? isHole (L 1 Hole Empty) ]
    -- TODO: more zipper unit tests
    ] ]
