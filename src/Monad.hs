-- | This module contains type classes for functors, applicative functors,
-- monads, monads that are monoids, and monad transformers.
--
-- TODO: lookup language extension for overriding do-notation
--
module Monad (
    Functor (..)
  , (<$>)

  , Applicative (..)
  , liftA
  , liftA2
  , sequenceA
  , when
  , unless
  ,  ZipList (..)

  , Monad (..)
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
    Monoid (..)
  , mconcat
  , Functor (..)
  , (<$>)
  , Applicative (..)
  , sequenceA
  , Monad (..)
  , sequence
  , (=<<)
  , (>>)
  , mapM
  )

import Monoid

infixl 1 >>=
infixl 1 >>

-- -----------------------------------------------------------------------------
-- Functors

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
-- Functor derived operations

(<$>) :: Functor t => (a -> b) -> t a -> t b
(<$>) = fmap

-- -----------------------------------------------------------------------------
-- Applicative functors

-- | Type class for applicative functors.
--
-- Identity law:
--
-- prop> pure id <*> tx = tx
--
-- Homomorphism law:
--
-- prop> pure f <*> pure x = pure (f x)
--
-- Composition law:
--
-- prop> pure (.) <*> tu <*> tv <*> tw = tu <*> (tv <*> tw)
--
-- Application law:
--
-- prop> tf <*> pure x = pure ($ x) <*> tf
--
-- Relationship with functors:
--
-- prop> fmap f tx = pure f <*> tx = f <$> tx
--
class Functor t => Applicative t where
  -- Inject
  pure :: a -> t a
  -- Apply
  (<*>) :: t (a -> b) -> t a -> t b

-- -----------------------------------------------------------------------------
-- Applicative derived operations

-- | This function can be used define `fmap` in a boilerplate `Functor`
-- instance.
--
-- prop> liftA2 = fmap
--
liftA :: Applicative t => (a -> b) -> t a -> t b
liftA f tx = pure f <*> tx

liftA2 :: Applicative t => (a -> b -> c) -> t a -> t b -> t c
liftA2 f tx ty = f <$> tx <*> ty

sequenceA :: Applicative t => [t a] -> t [a]
sequenceA []         = pure []
sequenceA (tx : txs) = (:) <$> tx <*> sequenceA txs
--sequenceA = foldr (liftA2 (:)) (pure [])

when :: Applicative t => Bool -> t () -> t ()
when True  tx = tx
when False _  = pure ()

unless :: Applicative t => Bool -> t () -> t ()
unless p = when (not p)

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

-- | This function can be used to define `fmap` in a boilerplate `Functor`
-- instance.
--
-- prop> liftM = fmap
--
liftM :: Monad t => (a -> b) -> t a -> t b
liftM f tx = tx >>= return . f

liftM2 :: Monad t => (a -> b -> c) -> t a -> t b -> t c
liftM2 f tx ty = f <$> tx <*> ty
-- liftM2 f tx ty = tx >>= \ x -> ty >>= return . f x

sequence :: Monad t => [t a] -> t [a]
sequence = foldr mcons (return []) where
  mcons tx txs = tx >>= \ x -> txs >>= \ xs -> return (x : xs)

mapM :: Monad t => (a -> t b) -> [a] -> t [b]
mapM f = sequence . map f

-- | This function can be used to define `(<*>)` in a boilerplate `Applicative`
-- instance.
--
-- prop> ap = (<*>)
--
ap :: Monad t => t (a -> b) -> t a -> t b
ap tf tx = tf >>= \ f -> tx >>= return . f

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
-- Functor example instances

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

-- -----------------------------------------------------------------------------
-- Applicative example instances

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  Just f  <*> m = fmap f m

instance Applicative (Either a) where
  pure = Right
  Left e  <*> _ = Left e
  Right f <*> e = fmap f e

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (x, f) <*> (y, z) = (x `mappend` y, f z)

instance Applicative ((->) a) where
  pure = const
  f <*> g = \x -> f x (g x)

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Ord, Read, Show)

instance Applicative ZipList where
  pure = ZipList . repeat
  ZipList fs <*> ZipList xs = ZipList $ zipWith ($) fs xs

-- Boilerplate
instance Functor ZipList where
  fmap = liftA

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
