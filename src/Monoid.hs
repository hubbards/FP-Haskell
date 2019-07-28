{-# LANGUAGE FlexibleInstances #-}

-- | This module contains a type class for monoids.
module Monoid where

import Prelude hiding ( Monoid(..) )

-- | Type class for monoids.
--
--   Associativity law:
--
--   prop> x `mappend` y `mappend` z = x `mappend` (y `mappend` z)
--
--   Left identity law:
--
--   prop> mempty `mappend` x = x
--
--   Right identity law:
--
--   prop> x `mappend` mempty = x
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

-- | Integers under addition.
instance Monoid Int where
  mempty = 0
  mappend = (+)

-- | Booleans under symmetric difference.
instance Monoid Bool where
  mempty = False
  x `mappend` y = x && not y || y && not x

-- | Lists under concatination.
instance Monoid [a] where
  mempty = []
  mappend = (++)

-- | Functions (with same domain and codomain) under composition.
instance Monoid (a -> a) where
  mempty = id
  mappend = (.)
