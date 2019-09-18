{-# LANGUAGE TupleSections #-}

-- | This module contains a deep-embedded DSL for a calculator language with two
-- integer registers.
module Calculator where

import Control.Monad ( liftM2 )

import Control.Monad.Trans.State (
    State
  , runState
  , modify
  , gets
  )

-- -----------------------------------------------------------------------------
-- Syntax

-- | Expression
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

-- | Type synonym for register state
type Reg = (Int, Int)

-- | Set value of first register
--
-- >>> set1 3 (1,2)
-- (3,2)
--
-- NOTE: need TupleSections language extension
--
set1 :: Int -> Reg -> Reg
set1 i = (i,) . snd

-- | Set value of second register
--
-- >>> set2 3 (1,2)
-- (1,3)
--
-- NOTE: need TupleSections language extension
--
set2 :: Int -> Reg -> Reg
set2 i = (,i) . fst

-- | Monadic semantic function. The state monad is used to model the semantic
-- domain.
evalS :: Exp -> State Reg Int
evalS (Lit i)      = return i
evalS (Neg e)      = fmap negate (evalS e)
evalS (Add l r)    = liftM2 (+) (evalS l) (evalS r)
evalS (Mul l r)    = liftM2 (*) (evalS l) (evalS r)
evalS (SetIn1 i e) = modify (set1 i) >> evalS e
evalS (SetIn2 i e) = modify (set2 i) >> evalS e
evalS (Save1 e)    = do i <- evalS e; modify (set1 i); return i
evalS (Save2 e)    = do i <- evalS e; modify (set2 i); return i
evalS Load1        = gets fst
evalS Load2        = gets snd

-- | Semantic function. Evaluates an expression with both register values
-- initially set to zero.
--
-- >>> eval $ Add (Lit 2) (Save1 (Neg (Lit 1)))
-- (1,(-1,0))
--
-- >>> eval $ Add (Save1 (Lit 1)) (Save2 (Lit 2))
-- (3,(1,2))
--
-- >>> eval $ Mul (Save1 (Lit 2)) (Save2 Load1)
-- (4,(2,2))
--
-- >>> eval $ SetIn1 1 (SetIn2 2 (Add Load1 Load2))
-- (3,(1,2))
--
-- >>> eval $ SetIn1 1 (SetIn2 2 (Add Load1 (Save1 Load2)))
-- (3,(2,2))
--
eval :: Exp -> (Int, Reg)
eval = flip runState (0, 0) . evalS
