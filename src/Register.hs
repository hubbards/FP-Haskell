-- | This module contains a register ADT and a deep-embedded DSL for a
-- register-based language.
module Register (
    Register
  , set1
  , set2
  , Exp (..)
  , eval
  , eval'
  ) where

import Control.Monad ( liftM2 )
-- from transformer package
import Control.Monad.Trans.State (
    State
  , state
  , runState
  , modify
  , gets
  )

-- -----------------------------------------------------------------------------
-- Register ADT

-- | Type synonym for register data structure
type Register a b = (a, b)

-- | Set value of first register
set1 :: c -> Register a b -> Register c b
set1 x (_, y) = (x, y)

-- | Set value of second register
set2 :: c -> Register a b -> Register a c
set2 y (x, _) = (x, y)

-- -----------------------------------------------------------------------------
-- Syntax

-- | Data type for expressions
data Exp = Lit Int        -- literal integer
         | Neg Exp        -- additive inverse
         | Add Exp Exp    -- addition
         | Mul Exp Exp    -- multiplication
         | Load1          -- load first register value
         | Load2          -- load second register value
         | SetIn1 Int Exp -- set first register value in expression
         | SetIn2 Int Exp -- set second register value in expression
         | Save1 Exp      -- save value of expression to first register
         | Save2 Exp      -- save value of expression to second register
  deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- Semantics

-- | Monadic semantic function. The state monad is used to model the semantic
-- domain.
eval :: Exp -> State (Register Int Int) Int
eval (Lit i)      = return i
eval (Neg e)      = fmap negate (eval e)
eval (Add l r)    = liftM2 (+) (eval l) (eval r)
eval (Mul l r)    = liftM2 (*) (eval l) (eval r)
eval Load1        = gets fst
eval Load2        = gets snd
eval (SetIn1 i e) = set1' i >> eval e
eval (SetIn2 i e) = set2' i >> eval e
eval (Save1 e)    = do
  i <- eval e
  set1' i
  return i
eval (Save2 e)    = do
  i <- eval e
  set2' i
  return i

set1' :: a -> State (Register a b) ()
set1' = modify . set1

set2' :: b -> State (Register a b) ()
set2' = modify . set2

-- | Semantic function. Evaluates an expression with both register values
-- initially set to zero.
eval' :: Exp -> (Int, Register Int Int)
eval' = flip runState (0, 0) . eval
