-- | This module contains a stack ADT and a deep-embedded DSL for a stack-based
-- language.
module Stack (
    Stack
  , Error
  , empty
  , push
  , pop
  , Cmd (..)
  , Prog
  , evalCmd
  , evalCmd'
  , evalProg
  , evalProg'
  ) where

import Control.Monad ( liftM2 )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State (
    StateT (..)
  , state
  , get
  , put
  , modify
  , execStateT
  )

-- -----------------------------------------------------------------------------
-- Stack ADT

-- | Type synonym for stack data structure
type Stack a = [a]

-- | Type synonym for errors which might occur in a stack operation
type Error = String

-- | Error when a operation requiring a non-empty stack is performed on a empty
-- stack
empty :: Error
empty = "Operation requires stack to be non-empty but stack is empty"

-- | Push a value onto the top of a stack.
push :: Monad m => a -> StateT (Stack a) m ()
push x = modify (x :)

-- | Pop a value off of the top of a stack.
pop :: StateT (Stack a) (Either Error) a
pop = StateT pop' where
  pop' []       = Left empty
  pop' (x : xs) = Right (x, xs)
-- pop = do
--   xs <- get
--   case xs of
--     []        -> lift (Left empty)
--     (x : xs') -> do { put xs' ; return x }

-- -----------------------------------------------------------------------------
-- Syntax

-- | Data type for commands
data Cmd = Push Int -- push number
         | Neg      -- pop number and push its additive inverse
         | Add      -- pop two numbers and push their sum
         | Mult     -- pop two numbers and push their product
  deriving (Eq, Show)

-- | Type synonym for programs, which are sequences of commands
type Prog = [Cmd]

-- -----------------------------------------------------------------------------
-- Semantics

-- | Monadic semantic function for commands. The state monad transformer is used
-- to model the semantic domain.
evalCmd :: Cmd -> StateT (Stack Int) (Either Error) ()
evalCmd (Push i) = push i
evalCmd Neg      = fmap negate pop >>= push
evalCmd Add      = liftM2 (+) pop pop >>= push
evalCmd Mult     = liftM2 (*) pop pop >>= push

evalCmd' :: Cmd -> Stack Int -> Either Error (Stack Int)
evalCmd' = execStateT . evalCmd

-- | Monadic semantic function for programs. The state monad transformer is used
-- to model the semantic domain.
evalProg :: Prog -> StateT (Stack Int) (Either Error) ()
evalProg = mapM_ evalCmd

evalProg' :: Prog -> Stack Int -> Either Error (Stack Int)
evalProg' = execStateT . evalProg
