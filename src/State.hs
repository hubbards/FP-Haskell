-- | This module contains implementations of the state monad and the state monad
-- transformer.
module State where

import Prelude hiding (
    Monoid(..)
  , Functor(..)
  , Applicative(..)
  , Monad(..)
  )

import Monoid
import Functor
import Applicative
import Monad

-- -----------------------------------------------------------------------------
-- State monad

-- | Data type for effectful computation. The first type parameter represents a
-- (side) effect type. The second type parameter represents a result type.
--
-- In domain theory, this corresponds to a function domain for naming with
-- mutable variables.
data State s a = S (s -> (a, s))
--type State s a = StateT s Identity a

instance Monad (State s) where
  return x = S $ \ s -> (x, s)
  S c >>= f = S $ \ s -> let (x, t) = c s; S d = f x in d t

-- Boilerplate
instance Applicative (State s) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Functor (State s) where
  fmap = liftM

-- -----------------------------------------------------------------------------
-- State monad functions

-- | Set current state.
put :: s -> State s ()
put s = S $ \ _ -> ((), s)

-- | Get current state.
get :: State s s
get = S $ \ s -> (s, s)

-- | Run effectful computation with initial state and return effect and result
-- of computation.
runState :: State s a -> s -> (a, s)
runState (S c) s = c s

-- | Run effectful computation with initial state and return result.
evalState :: State s a -> s -> a
evalState sc s = fst (runState sc s)

-- | Run effectful computation with initial state and return effect.
execState :: State s a -> s -> s
execState sc s = snd (runState sc s)

-- TODO: add more primatives / functions from Control.Monad.Trans.State

-- -----------------------------------------------------------------------------
-- State monad transformer

-- | Data type for state monad transformer. The first and last type parameters
-- are the same as before and the second type parameter represents the inner
-- monad.
data StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift m = StateT $ \ s -> m >>= \ a -> return (a, s)

instance Monad m => Monad (StateT s m) where
  return x = StateT $ \ s -> return (x, s)
  StateT c >>= f = StateT $ \s -> c s >>= \ (x, t) -> let StateT d = f x in d t

-- Boilerplate
instance Monad m => Applicative (StateT s m) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Monad m => Functor (StateT s m) where
  fmap = liftM

-- -----------------------------------------------------------------------------
-- State monad transformer functions

-- TODO: add primatives / functions from Control.Monad.Trans.State
