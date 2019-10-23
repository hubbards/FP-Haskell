-- | This module contains implementations of the reader monad and the reader
-- monad transformer.
module Reader (
    Reader
  , reader
  , runReader
  , ask
  , local

  , ReaderT
  , readerT
  , runReaderT
  ) where

import Control.Monad (
    ap
  , liftM
  )

-- NOTE: from transformers package
import Control.Monad.Trans.Class ( MonadTrans (..) )

-- -----------------------------------------------------------------------------
-- Reader monad data type and type class instances

-- | Data type for reader. The first type parameter represents an environment
-- type, which is usually a function from names to bound values. The second
-- type parameter represents a result value type.
--
-- In domain theory, this corresponds to a function domain for naming with
-- immutable variables.
data Reader r a = R (r -> a)
-- type Reader r a = ReaderT r Identity a

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

reader :: (r -> a) -> Reader r a
reader = R

runReader :: Reader r a -> r -> a
runReader (R c) = c

ask :: Reader r r
ask = R id

local :: (r -> r) -> Reader r a -> Reader r a
local f (R c) = R (c . f)

-- -----------------------------------------------------------------------------
-- Reader monad transformer data type and type class instances

-- | Data type for reader monad transformer. The first and last type parameters
-- are the same as before and the second type parameter represents the inner
-- monad.
data ReaderT r t a = RT (r -> t a)
-- newtype ReaderT r t a = ReaderT { runReaderT :: r -> t a }

instance MonadTrans (ReaderT r) where
  lift = RT . const

instance Monad t => Monad (ReaderT r t) where
  return = lift . return
  RT c >>= f = RT $ \ r -> do { x <- c r ; let RT c' = f x in c' r }

-- Boilerplate
instance Monad t => Applicative (ReaderT r t) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Monad t => Functor (ReaderT r t) where
  fmap = liftM

-- -----------------------------------------------------------------------------
-- Reader monad transformer functions

readerT :: Monad t => (r -> t a) -> ReaderT r t a
readerT = RT

runReaderT :: ReaderT r t a -> r -> t a
runReaderT (RT c) = c

-- TODO: add primatives / functions (from Control.Monad.Trans.Reader)
