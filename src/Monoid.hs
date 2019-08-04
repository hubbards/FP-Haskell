{-# LANGUAGE FlexibleInstances #-}

-- | This module contains a type class for monoids.
module Monoid where

import Prelude hiding ( Monoid(..) )

-- | Type class for monoids.
--
-- Associativity law:
--
-- prop> x `mappend` y `mappend` z = x `mappend` (y `mappend` z)
--
-- Left identity law:
--
-- prop> mempty `mappend` x = x
--
-- Right identity law:
--
-- prop> x `mappend` mempty = x
--
class Monoid a where
  -- Identity
  mempty :: a
  -- Binary operator
  mappend :: a -> a -> a

-- -----------------------------------------------------------------------------
-- Derived operations

mconcat :: Monoid a => [a] -> a
mconcat = foldr mappend mempty

-- -----------------------------------------------------------------------------
-- Example instances

newtype Sum a = Sum { getSum :: a }
  deriving ( Eq, Ord, Bounded, Read, Show )

newtype Product a = Product { getProduct :: a }
  deriving ( Eq, Ord, Bounded, Read, Show )

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  Sum x `mappend` Sum y = Sum (x + y)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x * y)

newtype Any = Any { getAny :: Bool }
  deriving ( Eq, Ord, Bounded, Read, Show )

newtype All = All { getAll :: Bool }
  deriving ( Eq, Ord, Bounded, Read, Show )

instance Monoid Any where
  mempty = Any False
  Any x `mappend` Any y = Any (x || y)

instance Monoid All where
  mempty = All True
  All x `mappend` All y = All (x && y)

instance Monoid Bool where
  mempty = False
  x `mappend` y = x && not y || y && not x

instance Monoid Ordering where
  mempty = EQ
  mappend EQ = id
  mappend o  = const o

newtype First a = First { getFirst :: Maybe a }
  deriving ( Eq, Ord, Read, Show )

instance Monoid (First a) where
  mempty = First Nothing
  mappend (First Nothing) = id
  mappend f               = const f

instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` x       = x
  x       `mappend` Nothing = x
  Just x  `mappend` Just y  = Just (x `mappend` y)

instance Monoid [a] where
  mempty = []
  mappend = (++)

instance Monoid (a -> a) where
  mempty = id
  mappend = (.)
