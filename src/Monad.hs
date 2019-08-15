-- | This module contains type classes for monads, monads that are monoids, and
-- monad transformers.
--
-- TODO: lookup language extension for overriding do-notation
--
module Monad (
    Monad (..)
  , join
  , (>>)
  , (=<<)
  , liftM
  , liftM2
  , sequence
  , mapM
  , ap
  , MonadPlus (..)
  , guard
  , MonadTrans (..)
  , MaybeT (..)
  ) where

import Prelude hiding (
    Functor (..)
  , Applicative (..)
  , sequenceA
  , (<$>)
  , Monad (..)
  , sequence
  , (=<<)
  , (>>)
  , mapM
  )

import Functor
import Applicative

infixl 1 >>=
infixl 1 >>

-- -----------------------------------------------------------------------------
-- Monads

-- | Type class for monads.
--
-- Left identity law:
--
-- prop> return x >>= f = f x
--
-- Right identity law:
--
-- prop> tx >>= return = tx
--
-- Associativity:
--
-- prop> (tx >>= f) >>= g = tx >>= (\x -> f x >>= g)
--
-- Relationship with functors:
--
-- prop> fmap = liftM
--
-- Relationship with applicative functors:
--
-- prop> (<*>) = ap
--
class Applicative t => Monad t where
  -- Inject
  return :: a -> t a
  return = pure
  -- Bind
  (>>=) :: t a -> (a -> t b) -> t b

-- -----------------------------------------------------------------------------
-- Monad derived operations

join :: Monad t => t (t a) -> t a
join x = x >>= id

(>>) :: Monad t => t a -> t b -> t b
tx >> ty = tx >>= const ty
-- tx >> ty = tx >>= \ _ -> ty

(=<<) :: Monad t => (a -> t b) -> t a -> t b
(=<<) = flip (>>=)

liftM :: Monad t => (a -> b) -> t a -> t b
liftM = (<$>)
-- liftM f tx = tx >>= return . f

liftM2 :: Monad t => (a -> b -> c) -> t a -> t b -> t c
liftM2 f tx ty = f <$> tx <*> ty
-- liftM2 f tx ty = tx >>= \ x -> ty >>= return . f x

sequence :: Monad t => [t a] -> t [a]
sequence = foldr mcons (return []) where
  mcons tx txs = tx >>= \ x -> txs >>= \ xs -> return (x : xs)

mapM :: Monad t => (a -> t b) -> [a] -> t [b]
mapM f = sequence . map f

ap :: Monad t => t (a -> b) -> t a -> t b
ap = (<*>)
-- ap tf tx = tf >>= \ f -> tx >>= return . f

-- -----------------------------------------------------------------------------
-- Monads that are monoids

-- | Type class for monads that are monoids.
--
-- Monoid laws:
--
-- prop> x `mplus` y `mplus` z = x `mplus` (y `mplus` z)
--
-- prop> mzero `mplus` x = x
--
-- prop> x `mplus` mzero = x
--
-- Failure propagation law:
--
-- prop> mzero >>= f = mzero
--
class Monad t => MonadPlus t where
  mzero :: t a
  mplus :: t a -> t a -> t a

-- -----------------------------------------------------------------------------
-- MonadPlus derived operations

guard :: MonadPlus t => Bool -> t ()
guard True  = return ()
guard False = mzero

-- -----------------------------------------------------------------------------
-- Monad transformers

-- | Type class for monad transformers.
--
-- Identity law:
--
-- prop> lift . return = return
--
-- Distributivity law:
--
-- prop> lift (m >>= f) = lift m >>= (lift . f)
--
class MonadTrans t where
  -- Lift
  lift :: Monad m => m a -> t m a

-- -----------------------------------------------------------------------------
-- Monad example instances

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x  >>= f = f x

instance Monad [] where
  []       >>= _ = []
  (x : xs) >>= f = f x ++ (xs >>= f)
-- (>>=) = flip concatMap

-- -----------------------------------------------------------------------------
-- MonadPlus example instances

instance MonadPlus Maybe where
  mzero = Nothing
  Just x  `mplus` _ = Just x
  Nothing `mplus` m = m

instance MonadPlus [] where
  mzero = []
  mplus = (++)

-- -----------------------------------------------------------------------------
-- MonadTrans example instances

-- TODO: add Control.Monad.Identity

-- | Data type for maybe monad transformer. The first type parameter represents
-- the inner monad.
data MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
  lift m = MaybeT (m >>= return . Just)

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  x >>= f =
    MaybeT $ runMaybeT x >>= \ m -> case m of
                                      Nothing -> return Nothing
                                      Just y  -> runMaybeT (f y)

-- Boilerplate
instance Monad m => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Monad m => Functor (MaybeT m) where
  fmap = liftM

instance Monad m => MonadPlus (MaybeT m) where
  mzero = MaybeT (return Nothing)
  x `mplus` y =
    MaybeT $ runMaybeT x >>= \ m -> case m of
                                      Nothing -> return m
                                      Just _  -> runMaybeT y

-- TODO: add primatives / functions from Control.Monad.Trans.Maybe
