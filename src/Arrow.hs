-- | This module contains a type class for arrows.
module Arrow (
    assoc
  , Arrow (..)
  , second
  , (***)
  , (&&&)
  , liftA2
  , Kleisli (..)
  ) where

import Data.Tuple ( swap )

assoc :: ((a, b), c) -> (a, (b, c))
assoc ((x, y), z) = (x, (y, z))

-- | Type class for arrows.
--
-- Identity laws:
--
-- > arr id >>> f = f
--
-- > f >>> arr id = f
--
-- Associativity:
--
-- > (f >>> g) >>> h = f >>> (g >>> h)
--
-- Distributivity of `arr` over `>>>`:
--
-- > arr (f >>> g) = arr f >>> arr g
--
-- Distributivity of `first` over `>>>`:
--
-- > first (f >>> g) = first f >>> first g
--
-- Definition of `first`:
--
-- > first f >>> arr fst = arr fst >>> f
--
-- > first f >>> second (arr g) = second (arr g) >>> first f
--
-- > arr assoc >>> first f = first (first f) >>> arr assoc
--
-- Commutivity of `first` and `arr`:
--
-- NOTE: the following law seems to only apply to the `->` instance.
--
-- > first (arr f) = arr (first f)
--
class Arrow t where
  -- lift a function into an arrow
  arr :: (a -> b) -> t a b
  -- arrow composition
  (>>>) :: t a b -> t b c -> t a c
  -- Convert to arrow on pairs which leaves second component unchanged
  first :: t a b -> t (a, c) (b, c)

-- -----------------------------------------------------------------------------
-- Derived operations

-- | Convert to arrow on pairs which leaves first component unchanged.
second :: Arrow t => t a b -> t (c, a) (c, b)
second f = arr swap >>> first f >>> arr swap

-- | Combinator which processes both components of a pair.
(***) :: Arrow t => t a b -> t c d -> t (a, c) (b, d)
f *** g = first f >>> second g

-- | Combinator which builds a pair from the result of two arrows.
(&&&) :: Arrow t => t a b -> t a c -> t a (b, c)
f &&& g = arr (\ x -> (x, x)) >>> (f *** g)

-- | Apply a binary operator to two arrows.
liftA2 :: Arrow t => (a -> b -> c) -> t d a -> t d b -> t d c
liftA2 op f g = (f &&& g) >>> arr (\ (x, y) -> x `op` y)

-- -----------------------------------------------------------------------------
-- Example instances

instance Arrow (->) where
  arr f = f
  -- arr = id
  f >>> g = g . f
  -- (>>>) = flip (.)
  first f (x, y) = (f x, y)

data Kleisli m a b = K (a -> m b)

instance Monad m => Arrow (Kleisli m) where
  arr f = K (return . f)
  -- arr f = K $ \ x -> return (f x)
  K f >>> K g = K $ \ x -> f x >>= g
  first (K f) = K $ \ (x, z) -> f x >>= \ y -> return (y, z)
