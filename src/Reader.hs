-- | This module contains implementations of the reader monad and the reader
-- monad transformer.
module Reader (
    Reader (..)
  , ask
  , local
  , ReaderT (..)
  ) where

import Prelude hiding (
    Functor(..)
  , Applicative(..)
  , Monad(..)
  )

import Functor
import Applicative
import Monad

-- -----------------------------------------------------------------------------
-- Reader monad data type and type class instances

-- | Data type for reader. The first type parameter represents an environment
-- type, which is usually a function from names to bound values. The second
-- type parameter represents a result value type.
--
-- In domain theory, this corresponds to a function domain for naming with
-- immutable variables.
data Reader r a = R (r -> a)
--type Reader r a = ReaderT r Identity a

instance Monad (Reader r) where
  return x = R (const x)
  R c >>= f = R $ \ r -> let x = c r; R d = f x in d r

-- Boilerplate
instance Applicative (Reader r) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Functor (Reader r) where
  fmap = liftM

-- -----------------------------------------------------------------------------
-- Reader monad functions

ask :: Reader r r
ask = R id

-- TODO: implement
local :: (r -> r) -> Reader r a -> Reader r a
local = undefined

-- TODO: add more functions

-- -----------------------------------------------------------------------------
-- Reader monad transformer data type and type class instances

-- | Data type for reader monad transformer. The first and last type parameters
-- are the same as before and the second type parameter represents the inner
-- monad.
data ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance MonadTrans (ReaderT r) where
  lift m = ReaderT (const m)

instance Monad m => Monad (ReaderT r m) where
  return = lift . return
  ReaderT c >>= f = ReaderT $ \ r -> c r >>= \ x -> let ReaderT d = f x in d r

-- Boilerplate
instance Monad m => Applicative (ReaderT r m) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Monad m => Functor (ReaderT r m) where
  fmap = liftM

-- -----------------------------------------------------------------------------
-- Reader monad transformer functions (from Control.Monad.Trans.Reader)

-- TODO: add primatives / functions from Control.Monad.Trans.Reader
