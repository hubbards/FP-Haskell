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
    (empty :: LQueue Int)           @=? fromList []
  , (queue1 :: LQueue Int)          @=? fromList [1]
  , (queue12 :: LQueue Int)         @=? fromList [1, 2]
  , dequeue (empty :: LQueue Int)   @=? Nothing
  , dequeue (queue1 :: LQueue Int)  @=? Just (1, empty)
  , dequeue (queue12 :: LQueue Int) @=? Just (1, fromList [2])
  -- TODO: add more unit tests
  ] where
    queue1  = enqueue 1 empty
    queue12 = enqueue 2 queue1

testBQueue :: Test
testBQueue = "BQueue" ~: [
    (empty :: BQueue Int)           @=? fromList []
  , (queue1 :: BQueue Int)          @=? fromList [1]
  , (queue12 :: BQueue Int)         @=? fromList [1, 2]
  , dequeue (empty :: BQueue Int)   @=? Nothing
  , dequeue (queue1 :: BQueue Int)  @=? Just (1, empty)
  , dequeue (queue12 :: BQueue Int) @=? Just (1, fromList [2])
  -- TODO: add more unit tests
  ] where
    queue1  = enqueue 1 empty
    queue12 = enqueue 2 queue1

testPQueue :: Test
testPQueue = "PQueue" ~: [
    (empty :: PQueue Int)           @=? fromList []
  , (queue1 :: PQueue Int)          @=? fromList [1]
  , (queue12 :: PQueue Int)         @=? fromList [1, 2]
  , dequeue (empty :: PQueue Int)   @=? Nothing
  , dequeue (queue1 :: PQueue Int)  @=? Just (1, empty)
  , dequeue (queue12 :: PQueue Int) @=? Just (1, fromList [2])
  -- TODO: add more unit tests
  ] where
    queue1  = enqueue 1 empty
    queue12 = enqueue 2 queue1
