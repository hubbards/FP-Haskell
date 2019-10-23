-- | This module contains implementations of the state monad and the state monad
-- transformer.
module State (
    State
  , state
  , runState
  , evalState
  , execState
  , put
  , get
  , modify

  , StateT
  , stateT
  , runStateT
  ) where

import Control.Monad (
    ap
  , liftM
  )

-- NOTE: from transformers package
import Control.Monad.Trans.Class ( MonadTrans (..) )

-- -----------------------------------------------------------------------------
-- State monad data type and type class instances

-- | Data type for effectful computation. The first type parameter represents a
-- (side) effect type. The second type parameter represents a result type.
--
-- In domain theory, this corresponds to a function domain for naming with
-- mutable variables.
data State s a = S (s -> (a, s))
-- type State s a = StateT s Identity a

instance Monad (State s) where
  return x = S $ \ s -> (x, s)
  S c >>= f = S $ \ s -> let (x, s') = c s; S c' = f x in c' s'

-- Boilerplate
instance Applicative (State s) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Functor (State s) where
  fmap = liftM

-- -----------------------------------------------------------------------------
-- State monad functions

state :: (s -> (a, s)) -> State s a
state = S

-- | Run effectful computation with initial state and return effect and result
-- of computation.
runState :: State s a -> s -> (a, s)
runState (S c) = c

-- | Run effectful computation with initial state and return result.
evalState :: State s a -> s -> a
evalState sc s = fst (runState sc s)

-- | Run effectful computation with initial state and return effect.
execState :: State s a -> s -> s
execState sc s = snd (runState sc s)

-- | Set current state.
put :: s -> State s ()
put s = S $ const ((), s)

-- | Get current state.
get :: State s s
get = S $ \ s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = get >>= (put . f)

-- TODO: add more primatives / functions from Control.Monad.Trans.State

-- -----------------------------------------------------------------------------
-- State monad transformer data type and type class instances

-- | Data type for state monad transformer. The first and last type parameters
-- are the same as before and the second type parameter represents the inner
-- monad.
data StateT s t a = ST (s -> t (a, s))
-- newtype StateT s t a = StateT { runStateT :: s -> t (a, s) }

instance MonadTrans (StateT s) where
  lift v = ST $ \ s -> do { x <- v ; return (x, s) }

instance Monad t => Monad (StateT s t) where
  return x = ST $ \ s -> return (x, s)
  ST c >>= f = ST $ \ s -> do { (x, s') <- c s ; let ST c' = f x in c' s' }

-- Boilerplate
instance Monad t => Applicative (StateT s t) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Monad t => Functor (StateT s t) where
  fmap = liftM

-- -----------------------------------------------------------------------------
-- State monad transformer functions

stateT :: Monad t => (s -> t (a, s)) -> StateT s t a
stateT = ST

runStateT :: StateT s t a -> s -> t (a, s)
runStateT (ST c) = c

-- TODO: add primatives / functions (from Control.Monad.Trans.State)
