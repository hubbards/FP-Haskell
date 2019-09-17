-- | This module contains implementations of the writer monad and the writer
-- monad transformer.
module Writer (
    Writer (..)
  , tell
  , listen
  , listens
  , pass
  , censor
  , WriterT (..)
  ) where

import Control.Monad (
    ap
  , liftM
  , MonadPlus (..)
  )

import Control.Applicative ( Alternative (..) )

import Control.Monad.Trans.Class ( MonadTrans (..) )

-- -----------------------------------------------------------------------------
-- Writer monad data type and type class instances

-- | Data type for writer. The first type parameter represents the type of logs
-- (or tags). The second type parameter represents the type of values being
-- logged.
--
-- In domain theory, this corresponds to a product domain.
data Writer w a = W a w
--type Writer w a = WriterT w Identity a

instance Monoid w => Monad (Writer w) where
  return x = W x mempty
  W x s >>= f = let W y t = f x in W y (s `mappend` t)

-- Boilerplate
instance Monoid w => Applicative (Writer w) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Monoid w => Functor (Writer w) where
  fmap = liftM

-- -----------------------------------------------------------------------------
-- Writer monad functions

tell :: w -> Writer w ()
tell = W ()

-- TODO: implement
listen :: Writer w a -> Writer w (a, w)
listen = undefined
-- listen (W x s) = W (x, s) s

-- TODO: implement
listens :: (w -> b) -> Writer w a -> Writer w (a, b)
listens = undefined
-- listens f (W x s) = W (x, f s) s

-- TODO: implement
pass :: Writer w (a, w -> w) -> Writer w a
pass = undefined
-- pass (W (x, f) s) = W x (f s)

-- TODO: implement
censor :: (w -> w) -> Writer w a -> Writer w a
censor = undefined
-- censor f (W x s) = W x (f s)

-- -----------------------------------------------------------------------------
-- Writer monad transformer data type and type class instances

data WriterT w m a = WriterT { runWriterT :: m (a, w) }

-- TODO: implement
instance MonadTrans (WriterT w) where
  lift = undefined

-- TODO: implement
instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return = undefined
  (>>=) = undefined

-- Boilerplate
instance (Monoid w, Monad m) => Applicative (WriterT w m) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance (Monoid w, Monad m) => Functor (WriterT w m) where
  fmap = liftM

-- TODO: implement
instance (Monoid w, MonadPlus m) => Alternative (WriterT w m) where
  empty = undefined
  (<|>) = undefined

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
  mzero = WriterT mzero
  x `mplus` y = WriterT $ runWriterT x `mplus` runWriterT y

-- -----------------------------------------------------------------------------
-- Writer monad transformer functions (from Control.Monad.Trans.Writer)

-- TODO: add primatives / functions
