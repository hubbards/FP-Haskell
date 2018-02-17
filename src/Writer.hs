-- | This module contains implementations of the writer monad and the writer
--   monad transformer.
module Writer where

import Prelude hiding (
    Monoid(..)
  , Functor(..)
  , Applicative(..)
  , Monad(..))

import Monoid
import Functor
import Applicative
import Monad

-- -----------------------------------------------------------------------------
-- Writer monad data type and type class instances

-- | Data type for writer. The first type parameter represents the type of logs
--   (or tags). The second type parameter represents the type of values being
--   logged.
--
--   In domain theory, this corresponds to a product domain.
data Writer w a = W w a
--type Writer w a = WriterT w Identity a

-- TODO: comment
instance Monoid w => Monad (Writer w) where
  return = W mempty
  W s x >>= f = let W t y = f x in W (s `mappend` t) y

-- Boilerplate
instance Monoid w => Applicative (Writer w) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Monoid w => Functor (Writer w) where
  fmap = liftM

-- TODO: implement
instance Monoid w => MonadPlus (Writer w) where
  mzero = undefined
  mplus = undefined

-- -----------------------------------------------------------------------------
-- Writer monad functions

tell :: w -> Writer w ()
tell s = W s ()

-- TODO: implement
listen :: Writer w a -> Writer w (a, w)
listen = undefined

-- TODO: implement
pass :: Writer w (a, w -> w) -> Writer w a
pass = undefined

-- TODO: implement
censor :: (w -> w) -> Writer w a -> Writer w a
censor = undefined

-- TODO: add more functions

-- -----------------------------------------------------------------------------
-- Writer monad transformer data type and type class instances

data WriterT w m a = WriterT { runWriterT :: m (a, w) }

-- TODO: implement
instance Monoid w => MonadTrans (WriterT w) where
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

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
  mzero = WriterT mzero
  x `mplus` y = WriterT $ runWriterT x `mplus` runWriterT y

-- -----------------------------------------------------------------------------
-- Writer monad transformer functions (from Control.Monad.Trans.Writer)

-- TODO: add primatives / functions
