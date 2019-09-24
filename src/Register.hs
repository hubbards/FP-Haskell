-- | This module contains a register ADT and a deep-embedded DSL for a
-- register-based language.
module Register where

import Control.Monad ( liftM2 )
import Control.Monad.Trans.State (
    State
  , runState
  , modify
  , gets
  )

-- -----------------------------------------------------------------------------
-- Register ADT

-- | Type synonym for register data structure
type Register a b = (a, b)

-- | Set value of first register
set1 :: a -> Register a b -> Register a b
set1 x (_, y) = (x, y)

-- | Set value of second register
set2 :: b -> Register a b -> Register a b
set2 y (x, _) = (x, y)

-- -----------------------------------------------------------------------------
-- Syntax

-- | Data type for expressions
data Exp = Lit Int        -- literal integer
         | Neg Exp        -- additive inverse
         | Add Exp Exp    -- addition
         | Mul Exp Exp    -- multiplication
         | SetIn1 Int Exp -- set first register value in expression
         | SetIn2 Int Exp -- set second register value in expression
         | Save1 Exp      -- save value of expression to first register
         | Save2 Exp      -- save value of expression to second register
         | Load1          -- load first register value
         | Load2          -- load second register value
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
eval (SetIn1 i e) = modify (set1 i) >> eval e
eval (SetIn2 i e) = modify (set2 i) >> eval e
eval (Save1 e)    = do i <- eval e; modify (set1 i); return i
eval (Save2 e)    = do i <- eval e; modify (set2 i); return i
eval Load1        = gets fst
eval Load2        = gets snd

-- | Semantic function. Evaluates an expression with both register values
-- initially set to zero.
eval' :: Exp -> (Int, Register Int Int)
eval' = flip runState (0, 0) . eval
