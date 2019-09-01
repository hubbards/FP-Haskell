-- | This module contains implementations of the identity monad.
module FP.Control.Identity ( Identity (..) ) where

import Prelude hiding (
    Functor (..)
  , (<$>)
  , Applicative (..)
  , sequenceA
  , Monad (..)
  , sequence
  , (=<<)
  , (>>)
  , mapM
  )

import FP.Control.Monad

data Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  return = Identity
  Identity x >>= f = f x

-- Boilerplate
instance Applicative Identity where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Functor Identity where
  fmap = liftM