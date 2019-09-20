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
  , maybeT
  , runMaybeT
  ) where

import Prelude hiding (
    Functor (..)
  , (<$>)
  , Applicative (..)
  , sequenceA
  , Monad (..)
  , sequence
  , (=<<)
  , (>>)
  , mapM
  )

infixl 1 >>=
infixl 1 >>

-- TODO: fixity declarations for other operators

-- -----------------------------------------------------------------------------
-- Functors

-- | Type class for functors.
--
-- Identity law:
--
-- > fmap id = id
--
-- Map fusion law:
--
-- > fmap (g . f) = fmap g . fmap f
--
class Functor t where
  -- Map
  fmap :: (a -> b) -> t a -> t b

-- -----------------------------------------------------------------------------
-- Functor derived operations

(<$>) :: Functor t => (a -> b) -> t a -> t b
(<$>) = fmap

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
-- Applicative functors

-- | Type class for applicative functors.
--
-- Identity law:
--
-- > pure id <*> tx = tx
--
-- Homomorphism law:
--
-- > pure f <*> pure x = pure (f x)
--
-- Composition law:
--
-- > pure (.) <*> tu <*> tv <*> tw = tu <*> (tv <*> tw)
--
-- Application law:
--
-- > tf <*> pure x = pure ($ x) <*> tf
--
-- Relationship with functors:
--
-- > fmap f tx = pure f <*> tx = f <$> tx
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
-- > liftA2 = fmap
--
liftA :: Applicative t => (a -> b) -> t a -> t b
liftA f tx = pure f <*> tx

liftA2 :: Applicative t => (a -> b -> c) -> t a -> t b -> t c
liftA2 f tx ty = f <$> tx <*> ty

sequenceA :: Applicative t => [t a] -> t [a]
sequenceA = foldr (liftA2 (:)) (pure [])
-- sequenceA []         = pure []
-- sequenceA (tx : txs) = (:) <$> tx <*> sequenceA txs

when :: Applicative t => Bool -> t () -> t ()
when True  tx = tx
when False _  = pure ()

unless :: Applicative t => Bool -> t () -> t ()
unless p = when (not p)

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
-- Monads

-- | Type class for monads.
--
-- Left identity law:
--
-- > return x >>= f = f x
--
-- Right identity law:
--
-- > tx >>= return = tx
--
-- Associativity:
--
-- > (tx >>= f) >>= g = tx >>= (\x -> f x >>= g)
--
-- Relationship with functors:
--
-- > fmap = liftM
--
-- Relationship with applicative functors:
--
-- > (<*>) = ap
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

(=<<) :: Monad t => (a -> t b) -> t a -> t b
(=<<) = flip (>>=)

-- | This function can be used to define `fmap` in a boilerplate `Functor`
-- instance.
--
-- > liftM = fmap
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
-- > ap = (<*>)
--
ap :: Monad t => t (a -> b) -> t a -> t b
ap tf tx = tf >>= \ f -> tx >>= return . f

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
-- Monads that are monoids

-- | Type class for monads that are monoids.
--
-- Monoid laws:
--
-- > x `mplus` y `mplus` z = x `mplus` (y `mplus` z)
--
-- > mzero `mplus` x = x
--
-- > x `mplus` mzero = x
--
-- Failure propagation law:
--
-- > mzero >>= f = mzero
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
-- MonadPlus example instances

instance MonadPlus Maybe where
  mzero = Nothing
  Just x  `mplus` _ = Just x
  Nothing `mplus` m = m

instance MonadPlus [] where
  mzero = []
  mplus = (++)

-- -----------------------------------------------------------------------------
-- Monad transformers

-- | Type class for monad transformers.
--
-- Identity law:
--
-- > lift . return = return
--
-- Distributivity law:
--
-- > lift (m >>= f) = lift m >>= (lift . f)
--
class MonadTrans t where
  -- Lift
  lift :: Monad m => m a -> t m a

-- -----------------------------------------------------------------------------
-- MonadTrans example instances

-- TODO: add Control.Monad.Identity

-- | Data type for maybe monad transformer. The first type parameter represents
-- the inner monad.
data MaybeT m a = MT (m (Maybe a))

instance MonadTrans MaybeT where
  lift m = MT (Just <$> m)
-- lift m = MT (m >>= return . Just)

instance Monad m => Monad (MaybeT m) where
  return = MT . return . Just
  (MT x) >>= f =
    MT $ x >>= \ y -> case y of
                        Nothing -> return Nothing
                        Just z  -> runMaybeT (f z)

-- Boilerplate
instance Monad m => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Monad m => Functor (MaybeT m) where
  fmap = liftM

instance Monad m => MonadPlus (MaybeT m) where
  mzero = MT (return Nothing)
  (MT x) `mplus` (MT y) =
    MT $ x >>= \ z -> case z of
                        Nothing -> y
                        Just _  -> return z

maybeT :: Monad m => m (Maybe a) -> MaybeT m a
maybeT = MT

runMaybeT :: MaybeT m a -> m (Maybe a)
runMaybeT (MT x) = x

-- TODO: add primatives / functions (from Control.Monad.Trans.Maybe)
