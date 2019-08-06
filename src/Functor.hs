-- | This module contains a type class for functors.
module Functor ( Functor (..) ) where

import Prelude hiding ( Functor (..) )

-- -----------------------------------------------------------------------------
-- Functor

-- | Type class for functors.
--
-- Identity law:
--
-- prop> fmap id = id
--
-- Map fusion law:
--
-- prop> fmap (g . f) = fmap g . fmap f
--
class Functor t where
  -- Map
  fmap :: (a -> b) -> t a -> t b

-- -----------------------------------------------------------------------------
-- Derived operations

-- -----------------------------------------------------------------------------
-- Example instances

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Functor (Either a) where
  fmap _ (Left x)  = Left x
  fmap f (Right x) = Right (f x)

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

instance Functor ((->) a) where
  fmap = (.)

instance Functor [] where
  fmap = map
