-- | This module contains an dictionary pattern implementation of integer sets.
-- This implementation is based on William Cook's essay "On Understanding Data
-- Abstraction, Revisted".
module Set (
    Set (..)
  , insertObj
  , unionObj
  , empty
  , evens
  , odds
  ) where

-- | Data type for integer set interface.
data Set = S {
    isEmpty  :: Bool
  , contains :: Int -> Bool
  , insert   :: Int -> Set
  , union    :: Set -> Set
  }

-- | Helper function for inserting a number into a set.
insertObj :: Set -> Int -> Set
insertObj s x = this where
  this = S {
      isEmpty  = False
    , contains = \ y -> (x == y) || contains s y
    , insert   = insertObj this
    , union    = unionObj this
    }

-- | Helper function for unioning two sets.
unionObj :: Set -> Set -> Set
unionObj l r = this where
  this = S {
      isEmpty  = isEmpty l && isEmpty r
    , contains = \ x -> contains l x || contains r x
    , insert   = insertObj this
    , union    = unionObj  this
    }

-- | The empty set.
empty :: Set
empty = this where
  this = S {
      isEmpty  = True
    , contains = const False
    , insert   = insertObj this
    , union    = id
    }

-- | Set of even numbers.
evens :: Set
evens = this where
  this = S {
      isEmpty  = False
    , contains = even
    , insert   = insertObj this
    , union    = unionObj this
    }

-- | Set of odd numbers.
odds :: Set
odds = this where
  this = S {
      isEmpty  = False
    , contains = odd
    , insert   = insertObj this
    , union    = unionObj this
    }
