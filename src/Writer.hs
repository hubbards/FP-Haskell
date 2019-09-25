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

data WriterT w m a = WT (m (a, w))
-- newtype WriterT w m a = WriterT { runWriterT :: (m (a, w)) }

instance Monoid w => MonadTrans (WriterT w) where
  lift m = WT $ m >>= \ x -> return (x, mempty)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return x = WT $ return (x, mempty)
  WT m >>= f = WT $ m >>= \ (x, s) -> let WT n = f x in
                    n >>= \ (y, t) -> return (y, s `mappend` t)

-- Boilerplate
instance (Monoid w, Monad m) => Applicative (WriterT w m) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance (Monoid w, Monad m) => Functor (WriterT w m) where
  fmap = liftM

instance (Monoid w, MonadPlus m) => Alternative (WriterT w m) where
  empty = WT empty
  WT m <|> WT n = WT (m <|> n)

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
  mzero = WT mzero
  WT m `mplus` WT n = WT (m `mplus` n)

-- -----------------------------------------------------------------------------
-- Writer monad transformer functions

writerT :: (Monoid w, Monad m) => m (a, w) -> WriterT w m a
writerT = WT

runWriterT :: WriterT w m a -> m (a, w)
runWriterT (WT x) = x

-- TODO: add primatives / functions (from Control.Monad.Trans.Writer)
