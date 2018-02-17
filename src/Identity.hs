-- | This module contains implementations of the identity monad.
module Identity where

import Prelude hiding (
    Functor(..)
  , Applicative(..)
  , Monad(..))

import Functor
import Applicative
import Monad

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
