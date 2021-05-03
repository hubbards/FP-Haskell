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
  , (>>)
  , (=<<)
  , join
  , ap
  , liftM
  , liftM2
  , sequence
  , mapM
  , filterM
  , foldM

  , MonadPlus (..)
  , msum
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
  -- | Map
  fmap :: (a -> b) -> t a -> t b

-- -----------------------------------------------------------------------------
-- Functor derived operations

-- | Infix version of `fmap`.
--
-- This function is useful for writing applicative style function application.
-- For example, we can write `f <$> x <*> y` instead of `pure f <*> x <*> y`.
--
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
-- > pure id <*> v = v
--
-- Homomorphism law:
--
-- > pure f <*> pure x = pure (f x)
--
-- Composition law:
--
-- > pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--
-- Application law:
--
-- > v <*> pure x = pure ($ x) <*> v
--
-- Relationship with functors:
--
-- > liftA = fmap
--
class Functor t => Applicative t where
  -- | Inject
  pure :: a -> t a
  -- | Apply
  (<*>) :: t (a -> b) -> t a -> t b

-- -----------------------------------------------------------------------------
-- Applicative derived operations

-- | This function can be used define `fmap` in a boilerplate `Functor`
-- instance.
--
-- > liftA = fmap
--
liftA :: Applicative t => (a -> b) -> t a -> t b
liftA f v = pure f <*> v

liftA2 :: Applicative t => (a -> b -> c) -> t a -> t b -> t c
liftA2 f u v = f <$> u <*> v

sequenceA :: Applicative t => [t a] -> t [a]
sequenceA []       = pure []
sequenceA (v : vs) = (:) <$> v <*> sequenceA vs
-- sequenceA = foldr (liftA2 (:)) (pure [])

when :: Applicative t => Bool -> t () -> t ()
when True  tx = tx
when False _  = pure ()

unless :: Applicative t => Bool -> t () -> t ()
unless = when . not

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
-- NOTE: monads are a subclass of applicative functors. Prior to version 7.10.1
-- of GHC, this wasn't the case.
--
-- Left identity law:
--
-- > return x >>= f = f x
--
-- Right identity law:
--
-- > v >>= return = v
--
-- Associativity:
--
-- > (v >>= f) >>= g = v >>= (\ x -> f x >>= g)
--
-- Relationship with functors:
--
-- > liftM = fmap
--
-- Relationship with applicative functors:
--
-- > return = pure
--
-- > ap = (<*>)
--
class Applicative t => Monad t where
  -- | Inject
  return :: a -> t a
  return = pure
  -- | Bind
  (>>=) :: t a -> (a -> t b) -> t b

-- -----------------------------------------------------------------------------
-- Monad derived operations

(>>) :: Monad t => t a -> t b -> t b
tx >> ty = tx >>= const ty

(=<<) :: Monad t => (a -> t b) -> t a -> t b
(=<<) = flip (>>=)

-- | Flatten a monadic value within a monadic value.
--
-- > v >>= f = join (fmap f v)
--
join :: Monad t => t (t a) -> t a
join v = v >>= id

-- | This function can be used to define `<*>` in a boilerplate `Applicative`
-- instance.
--
-- > ap = (<*>)
--
ap :: Monad t => t (a -> b) -> t a -> t b
ap u v = u >>= \ f -> v >>= return . f

-- | This function can be used to define `fmap` in a boilerplate `Functor`
-- instance.
--
-- > liftM = fmap
--
liftM :: Monad t => (a -> b) -> t a -> t b
liftM f v = v >>= return . f

-- NOTE: liftM2 = liftA2
liftM2 :: Monad t => (a -> b -> c) -> t a -> t b -> t c
liftM2 f u v = u >>= \ x -> v >>= return . f x

-- NOTE: sequence = sequenceA
sequence :: Monad t => [t a] -> t [a]
sequence []       = return []
sequence (v : vs) = v >>= \ x -> sequence vs >>= return . (x :)
-- sequence = foldr (liftM2 (:)) (return [])

mapM :: Monad t => (a -> t b) -> [a] -> t [b]
mapM f = sequence . map f

-- TODO: implement
filterM :: Monad t => (a -> t Bool) -> [a] -> t [a]
filterM = undefined

-- TODO: implement
foldM :: Monad t => (a -> b -> t a) -> a -> [b] -> t a
foldM = undefined

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
  -- | Identity
  mzero :: t a
  -- | Binary operator
  mplus :: t a -> t a -> t a

-- -----------------------------------------------------------------------------
-- MonadPlus derived operations

msum :: MonadPlus t => [t a] -> t a
msum = foldr mplus mzero

-- NOTE: this is actually a derived operation of Alternative
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
-- > lift (v >>= f) = lift v >>= (lift . f)
--
class MonadTrans s where
  -- | Lift
  lift :: Monad t => t a -> s t a

-- -----------------------------------------------------------------------------
-- MonadTrans example instances

-- TODO: add Control.Monad.Identity

-- | Data type for maybe monad transformer. The first type parameter represents
-- the inner monad.
data MaybeT t a = MT (t (Maybe a))
-- newtype MaybeT t a = MaybeT { runMaybeT :: t (Maybe a) }

instance MonadTrans MaybeT where
  lift = MT . fmap Just
-- lift v = MT (Just <$> v)
-- lift v = MT (v >>= return . Just)

instance Monad t => Monad (MaybeT t) where
  return = MT . return . Just
  MT v >>= f =
    MT $ v >>= \ m -> case m of
                        Nothing -> return Nothing
                        Just x  -> runMaybeT (f x)

-- Boilerplate
instance Monad t => Applicative (MaybeT t) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Monad t => Functor (MaybeT t) where
  fmap = liftM

instance Monad t => MonadPlus (MaybeT t) where
  mzero = MT (return Nothing)
  MT l `mplus` MT r =
    MT $ l >>= \ m -> case m of
                        Nothing -> r
                        Just _  -> return m

maybeT :: Monad t => t (Maybe a) -> MaybeT t a
maybeT = MT

runMaybeT :: MaybeT t a -> t (Maybe a)
runMaybeT (MT v) = v

-- TODO: add primatives / functions (from Control.Monad.Trans.Maybe)
