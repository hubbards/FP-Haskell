-- | This module contains a type class for applicative functors.
module Applicative (
    Applicative (..)
  , (<$>)
  , liftA2
  , sequenceA
  , when
  , unless
  ,  ZipList (..)
  ) where

import Prelude hiding (
    Monoid (..)
  , Functor (..)
  , Applicative (..)
  , (<$>)
  , sequenceA
  )

import Monoid
import Functor

-- -----------------------------------------------------------------------------
-- Applicative functors

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

liftA2 :: Applicative t => (a -> b -> c) -> t a -> t b -> t c
liftA2 f tx ty = f <$> tx <*> ty

sequenceA :: Applicative t => [t a] -> t [a]
sequenceA []         = pure []
sequenceA (tx : txs) = (:) <$> tx <*> sequenceA txs
--sequenceA = foldr (liftA2 (:)) (pure [])

when :: Applicative t => Bool -> t () -> t ()
when True  tx = tx
when False _  = pure ()

unless :: Applicative t => Bool -> t () -> t ()
unless p = when (not p)

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

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Ord, Read, Show)

instance Applicative ZipList where
  pure = ZipList . repeat
  ZipList fs <*> ZipList xs = ZipList $ zipWith ($) fs xs

-- Boilerplate
instance Functor ZipList where
  fmap = (<$>)
