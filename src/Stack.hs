{-# LANGUAGE FlexibleContexts #-}

-- | This module contains a deep-embedded DSL for a stack-based command
-- language.
module Stack where

import Control.Monad ( liftM2 )

import Control.Monad.Except (
    MonadError (..)
  , throwError
  )

import Control.Monad.State (
    MonadState (..)
  , get
  , modify
  )

-- import Control.Monad.Trans.Except
-- import Control.Monad.Trans.Maybe

-- -----------------------------------------------------------------------------
-- Syntax

-- | Type synonym for stack programs
type Prog = [Cmd]

-- | Data type for stack commands
data Cmd = PushB Bool   -- push boolean
         | PushI Int    -- push integer
         | Not          -- pop boolean and push its negation
         | And          -- pop two booleans and push their conjunction
         | Or           -- pop two booleans and push their disjunction
         | Neg          -- pop integer and push its negation
         | Add          -- pop two integers and push their sum
         | Mul          -- pop two integers and push their product
         | If Prog Prog -- pop boolean and conditionally run one of two programs
  deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- Semantics

-- | Type synonym for program stack
type Stack = [Either Bool Int]

-- | Data type for error result
data Error = Empty
           | NotB
           | NotI
  deriving (Eq, Show)

-- TODO: document
--
-- NOTE: need FlexibleContexts language extension
--
pushB :: MonadState Stack m => Bool -> m ()
pushB b = modify (Left b :) -- do s <- get; put (Left b : s)

-- TODO: document
--
-- NOTE: need FlexibleContexts language extension
--
pushI :: MonadState Stack m => Int -> m ()
pushI i = modify (Right i :) -- do s <- get; put (Right i : s)

-- TODO: document
--
-- NOTE: need FlexibleContexts language extension
--
popB :: (MonadError Error m, MonadState Stack m) => m Bool
popB = do s <- get
          case s of
            []            -> throwError Empty
            (Left b : _)  -> return b
            (Right _ : _) -> throwError NotB

-- TODO: document
--
-- NOTE: need FlexibleContexts language extension
--
popI :: (MonadError Error m, MonadState Stack m) => m Int
popI = do s <- get
          case s of
            []            -> throwError Empty
            (Left _ : _)  -> throwError NotI
            (Right i : _) -> return i

-- | Monadic semantic function for stack commands.
--
-- TODO: doctests
--
-- NOTE: need FlexibleContexts language extension
--
evalCmd :: (MonadError Error m, MonadState Stack m) => Cmd -> m ()
evalCmd (PushB b)  = pushB b
evalCmd (PushI i)  = pushI i
evalCmd Not        = popB >>= pushB . not
evalCmd And        = liftM2 (&&) popB popB >>= pushB
evalCmd Or         = liftM2 (||) popB popB >>= pushB
evalCmd Neg        = popI >>= pushI . negate
evalCmd Add        = liftM2 (+) popI popI >>= pushI
evalCmd Mul        = liftM2 (*) popI popI >>= pushI
evalCmd (If p1 p2) = do b <- popB
                        let p = if b then p1 else p2
                        evalProg p

-- | Monadic semantic function for stack programs.
--
-- TODO: doctests
--
-- NOTE: need FlexibleContexts language extension
--
evalProg :: (MonadError Error m, MonadState Stack m) => Prog -> m ()
evalProg = mapM_ evalCmd
