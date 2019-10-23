-- | This module contains implementations of the writer monad and the writer
-- monad transformer.
module Writer (
    Writer
  , tell
  , listen
  , pass
  , censor

  , WriterT
  , writerT
  , runWriterT
  ) where

import Control.Monad (
    ap
  , liftM
  , MonadPlus (..)
  )
import Control.Applicative ( Alternative (..) )

-- NOTE: from transformers package
import Control.Monad.Trans.Class ( MonadTrans (..) )

-- -----------------------------------------------------------------------------
-- Writer monad data type and type class instances

-- | Data type for writer. The first type parameter represents the type of logs
-- (or tags). The second type parameter represents the type of values being
-- logged.
--
-- In domain theory, this corresponds to a product domain.
data Writer w a = W a w
-- type Writer w a = WriterT w Identity a

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

listen :: Writer w a -> Writer w (a, w)
listen (W x s) = W (x, s) s

pass :: Writer w (a, w -> w) -> Writer w a
pass (W (x, f) s) = W x (f s)

censor :: (w -> w) -> Writer w a -> Writer w a
censor f (W x s) = W x (f s)

-- -----------------------------------------------------------------------------
-- Writer monad transformer data type and type class instances

data WriterT w t a = WT (t (a, w))
-- newtype WriterT w t a = WriterT { runWriterT :: (t (a, w)) }

instance Monoid w => MonadTrans (WriterT w) where
  lift v = WT $ do { x <- v ; return (x, mempty) }

instance (Monoid w, Monad t) => Monad (WriterT w t) where
  return x = WT $ return (x, mempty)
  WT u >>= f = WT $ do
    (x, s) <- u
    let WT v = f x
    (y, t) <- v
    return (y, s `mappend` t)

-- Boilerplate
instance (Monoid w, Monad t) => Applicative (WriterT w t) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance (Monoid w, Monad t) => Functor (WriterT w t) where
  fmap = liftM

instance (Monoid w, MonadPlus t) => Alternative (WriterT w t) where
  empty = WT empty
  WT u <|> WT v = WT (u <|> v)

instance (Monoid w, MonadPlus t) => MonadPlus (WriterT w t) where
  mzero = WT mzero
  WT u `mplus` WT v = WT (u `mplus` v)

-- -----------------------------------------------------------------------------
-- Writer monad transformer functions

writerT :: (Monoid w, Monad t) => t (a, w) -> WriterT w t a
writerT = WT

runWriterT :: WriterT w t a -> t (a, w)
runWriterT (WT v) = v

-- TODO: add primatives / functions (from Control.Monad.Trans.Writer)
