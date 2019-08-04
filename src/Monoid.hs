{-# LANGUAGE FlexibleInstances #-}

-- | This module contains a type class for monoids.
module Monoid where

import Prelude hiding (
    Monoid(..)
  , sum
  , product
  , or
  , and
  )

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

-- | New type for treating a @Num@ type as a monoid under addition.
newtype Sum a = Sum { getSum :: a }
  deriving ( Eq, Ord, Bounded, Read, Show )

-- | New type for treating a @Num@ type as a monoid under multiplication.
newtype Product a = Product { getProduct :: a }
  deriving ( Eq, Ord, Bounded, Read, Show )

-- | A @Num@ type is a monoid under addition.
instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  Sum x `mappend` Sum y = Sum (x + y)

-- | A @Num@ type is a monoid under multiplication.
instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x * y)

-- | The @sum@ function derived from @mconcat@.
sum :: Num a => [a] -> a
sum = getSum . mconcat . map Sum

-- | The @product@ function derived from @mconcat@.
product :: Num a => [a] -> a
product = getProduct . mconcat . map Product

-- | New type for treating @Bool@ as a monoid under disjunction (OR).
newtype Any = Any { getAny :: Bool }
  deriving ( Eq, Ord, Bounded, Read, Show )

-- | New type for treating @Bool@ as a monoid under conjunction (AND).
newtype All = All { getAll :: Bool }
  deriving ( Eq, Ord, Bounded, Read, Show )

-- | @Bool@ is a monoid under disjunction (OR).
instance Monoid Any where
  mempty = Any False
  Any x `mappend` Any y = Any (x || y)

-- | @Bool@ is a monoid under conjunction (AND).
instance Monoid All where
  mempty = All True
  All x `mappend` All y = All (x && y)

-- | The @or@ function derived from @mconcat@.
or :: [Bool] -> Bool
or = getAny . mconcat . map Any

-- | The @and@ function derived from @mconcat@.
and :: [Bool] -> Bool
and = getAll . mconcat . map All

-- | @Bool@ is a monoid under symmetric difference (XOR).
instance Monoid Bool where
  mempty = False
  x `mappend` y = x && not y || y && not x

-- | @Ordering@ is a monoid.
instance Monoid Ordering where
  mempty = EQ
  LT `mappend` _ = LT
  EQ `mappend` y = y
  GT `mappend` _ = GT

-- | New type for treating a @Maybe@ type as a monoid.
newtype First a = First { getFirst :: Maybe a }
  deriving ( Eq, Ord, Read, Show )

-- | A @Maybe@ type is a monoid.
instance Monoid (First a) where
  mempty = First Nothing
  First (Just x) `mappend` _ = First (Just x)
  First Nothing  `mappend` x = x

-- | A @Maybe@ type is a monoid when the type parameter is a monoid.
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` x       = x
  x       `mappend` Nothing = x
  Just x  `mappend` Just y  = Just (x `mappend` y)

-- | A list type is a monoid under concatination.
instance Monoid [a] where
  mempty = []
  mappend = (++)

-- | A function type (with same domain and codomain) is a monoid under function
-- composition. Note that this requires the @FlexibleInstances@ language
-- extension.
instance Monoid (a -> a) where
  mempty = id
  mappend = (.)
