module StackTest ( tests ) where

-- NOTE: from HUnit package
import Test.HUnit (
    Test
  , (~:)
  , (@=?)
  )

import Stack

tests :: Test
tests = "Stack" ~:
  [ "evalCmd" ~:
    [ "Push 1" ~:
      [ Right [1]    @=? evalCmd' (Push 1) []
      , Right [1, 2] @=? evalCmd' (Push 1) [2] ]
    , "Add" ~:
      [ Right [3]    @=? evalCmd' Add [1, 2]
      , Right [3, 3] @=? evalCmd' Add [1 .. 3]
      , Left empty   @=? evalCmd' Add [1]
      , Left empty   @=? evalCmd' Add [] ] ]
  , "evalProg" ~:
    [ "Push 1; Push 2; Add" ~:
      [ Right [3]    @=? evalProg' [Push 1, Push 2, Add] []
      , Right [3, 1] @=? evalProg' [Push 1, Push 2, Add] [1] ]
    , "Push 1; Add" ~:
      [ Right [3]    @=? evalProg' [Push 1, Add] [2]
      , Right [3, 3] @=? evalProg' [Push 1, Add] [2, 3]
      , Left empty   @=? evalProg' [Push 1, Add] [] ] ] ]
