module QueueTest ( tests ) where

-- NOTE: from HUnit package
import Test.HUnit (
    Test
  , (~:)
  , (@=?)
  , (@?)
  )

import Queue

tests :: Test
tests = "Queue" ~: [
    testLQueue
  , testBQueue
  , testPQueue
  ]

testLQueue :: Test
testLQueue = "LQueue" ~: [
    fromList []            @=? (empty :: LQueue Int)
  , fromList [1]           @=? queue1
  , fromList [1, 2]        @=? queue12
  , Nothing                @=? dequeue (empty :: LQueue Int)
  , Just (1, empty)        @=? dequeue queue1
  , Just (1, fromList [2]) @=? dequeue queue12
  -- TODO: add more unit tests
  ] where
    queue1, queue12 :: LQueue
    queue1  = enqueue 1 empty
    queue12 = enqueue 2 queue1

testBQueue :: Test
testBQueue = "BQueue" ~: [
    fromList []            @=? (empty :: BQueue Int)
  , fromList [1]           @=? queue1
  , fromList [1, 2]        @=? queue12
  , Nothing                @=? dequeue (empty :: BQueue Int)
  , Just (1, empty)        @=? dequeue queue1
  , Just (1, fromList [2]) @=? dequeue queue12
  -- TODO: add more unit tests
  ] where
    queue1, queue12 :: BQueue Int
    queue1  = enqueue 1 empty
    queue12 = enqueue 2 queue1

testPQueue :: Test
testPQueue = "PQueue" ~: [
    fromList []            @=? (empty :: BQueue Int)
  , fromList [1]           @=? queue1
  , fromList [1, 2]        @=? queue12
  , Nothing                @=? dequeue (empty :: BQueue Int)
  , Just (1, empty)        @=? dequeue queue1
  , Just (1, fromList [2]) @=? dequeue queue12
  -- TODO: add more unit tests
  ] where
    queue1, queue12 :: PQueue Int
    queue1  = enqueue 1 empty
    queue12 = enqueue 2 queue1
