-- | This module contains a type class for applicative functors.
module Applicative where

import Prelude hiding (
    Monoid(..)
  , Functor(..)
  , Applicative(..)
  )

import Monoid
import Functor

-- | Type class for applicative functors.
--
-- Identity law:
--
-- prop> pure id <*> tx = tx
--
-- Homomorphism law:
--
-- prop> pure f <*> pure x = pure (f x)
--
-- Composition law:
--
-- prop> pure (.) <*> tu <*> tv <*> tw = tu <*> (tv <*> tw)
--
-- Application law:
--
-- prop> tf <*> pure x = pure ($ x) <*> tf
--
-- Relationship with functors:
--
-- prop> fmap f x = pure f <*> x = f <$> x
--
class Functor t => Applicative t where
  -- Inject
  pure :: a -> t a
  -- Apply
  (<*>) :: t (a -> b) -> t a -> t b

-- -----------------------------------------------------------------------------
-- Derived operations

(<$>) :: Functor t => (a -> b) -> t a -> t b
(<$>) = fmap

-- -----------------------------------------------------------------------------
-- Example instances

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  Just f  <*> m = fmap f m

instance Applicative (Either a) where
  pure = Right
  Left e  <*> _ = Left e
  Right f <*> e = fmap f e

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (x, f) <*> (y, z) = (x `mappend` y, f z)

instance Applicative ((->) a) where
  pure = const
  f <*> g = \x -> f x (g x)

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
