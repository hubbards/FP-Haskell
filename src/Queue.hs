-- | TODO: document
module Queue (
    Queue (..)
  , fromList
  , safeSkip
  , LQueue
  , BQueue
  , PQueue
  ) where

import Data.Function ( on )
import Data.Foldable ( toList )

-- | Type class for queue ADT
class Queue t where
  -- | Empty queue
  empty :: t a
  -- | Enqueue operation
  enqueue :: a -> t a -> t a
  -- | Dequeue operation; return 'Nothing' if queue is 'empty'
  dequeue :: t a -> Maybe (a, t a)

-- | Enqueue all elements from a list in order.
fromList :: Queue t => [a] -> t a
fromList = foldl (flip enqueue) empty

-- | Dequeue an element and discard it, or do nothing if 'empty'.
safeSkip :: Queue t => t a -> t a
safeSkip q = maybe q snd (dequeue q)

-- -----------------------------------------------------------------------------
-- List Queue

-- | New type for list implementation of queue
newtype LQueue a = LQ [a]
  deriving (Eq, Ord, Read, Show)

instance Queue LQueue where
  empty = LQ []

  -- NOTE: takes @O(n)@ time
  enqueue x (LQ xs) = LQ (xs ++ [x])

  -- NOTE: takes @O(1)@ time
  dequeue (LQ [])       = Nothing
  dequeue (LQ (x : xs)) = Just (x, LQ xs)

-- -----------------------------------------------------------------------------
-- Batched Queue

-- | Data type for batched queue.
--
-- NOTE: splits the queue into two lists for the front and back
--
data BQueue a = BQ [a] [a]

instance Queue BQueue where
  empty = BQ [] []

  -- NOTE: takes @O(1)@ time
  enqueue x (BQ l r) = BQ l (x : r)

  -- NOTE: takes worst case @O(n)@ time and amortized @O(1)@ time
  dequeue (BQ [] [])     = Nothing
  dequeue (BQ (x : l) r) = Just (x, BQ l r)
  dequeue (BQ [] r)      = dequeue $ BQ (reverse r) []

instance Foldable BQueue where
  foldr f z (BQ l r) = foldr f z (l ++ reverse r)

instance Eq a => Eq (BQueue a) where
  (==) = on (==) toList

instance Show a => Show (BQueue a) where
  show = show . toList

-- -----------------------------------------------------------------------------
-- Persistent Batched Queue

-- | Data type for persistent batched queue.
--
-- NOTE: records lengths of the front and back
--
data PQueue a = PQ Int Int [a] [a]

-- | Redistribute two lists, if necessary.
--
-- NOTE: else case will trigger when `n = m + 1`
--
balance :: PQueue a -> PQueue a
balance q@(PQ m n l r) =
  if n <= m
    then q
    else PQ (m + n) 0 (l ++ reverse r) []

instance Queue PQueue where
  empty = PQ 0 0 [] []

  -- NOTE: takes amortized @O(1)@ time
  enqueue x (PQ m n l r) = balance $ PQ m (n + 1) l (x : r)

  -- NOTE: takes amortized @O(1)@ time
  dequeue (PQ _ _ [] _)      = Nothing
  dequeue (PQ m n (x : l) r) = Just (x, balance $ PQ (m - 1) n l r)

instance Foldable PQueue where
  foldr f z (PQ _ _ l r) = foldr f z (l ++ reverse r)

instance Eq a => Eq (PQueue a) where
  (==) = on (==) toList

instance Show a => Show (PQueue a) where
  show = show . toList
