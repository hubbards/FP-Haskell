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

-- $setup
-- >>> import Data.Monoid ( Sum (..), Any (..) )

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

-- | @Functor@ instance for @Maybe@.
--
-- >>> fmap (+ 1) (Just 2)
-- Just 3
--
-- >>> fmap (+ 1) Nothing
-- Nothing
--
-- TODO: quickcheck properties for typeclass laws
--
instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

-- | @Functor@ instance for @Either a@.
--
-- >>> fmap (+ 1) (Right 2)
-- Right 3
--
-- >>> fmap (+ 1) (Left 2)
-- Left 2
--
-- >>> fmap (+ 1) (Left True)
-- Left True
--
-- TODO: quickcheck properties for typeclass laws
--
instance Functor (Either a) where
  fmap _ (Left x)  = Left x
  fmap f (Right x) = Right (f x)

-- | @Functor@ instance for @(a, )@.
--
-- >>> fmap (+ 1) (2, 3)
-- (2,4)
--
-- >>> fmap (+ 1) (True, 2)
-- (True,3)
--
-- TODO: quickcheck properties for typeclass laws
--
instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

-- | @Functor@ instance for @a -> @.
--
-- >>> fmap (+ 1) (* 2) 3
-- 7
--
-- TODO: quickcheck properties for typeclass laws
--
instance Functor ((->) a) where
  fmap = (.)

-- | @Functor@ instance for @[]@.
--
-- >>> fmap (+ 1) [1 .. 3]
-- [2,3,4]
--
-- >>> fmap (+ 1) []
-- []
--
-- TODO: quickcheck properties for typeclass laws
--
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

-- | @Applicative@ instance for @Maybe@.
--
-- >>> pure 1 :: Maybe Int
-- Just 1
--
-- >>> Just (+ 1) <*> Just 2
-- Just 3
--
-- >>> Just (+ 1) <*> Nothing
-- Nothing
--
-- >>> Nothing <*> Just 2
-- Nothing
--
-- >>> Nothing <*> Nothing
-- Nothing
--
-- TODO: quickcheck properties for typeclass laws
--
instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  Just f  <*> m = fmap f m

-- | @Applicative@ instance for @Either a@.
--
-- >>> pure 1 :: Either a Int
-- Right 1
--
-- >>> Right (+ 1) <*> Right 2
-- Right 3
--
-- >>> Right (+ 1) <*> Left 2
-- Left 2
--
-- >>> Right (+ 1) <*> Left True
-- Left True
--
-- >>> Left True <*> Right 2
-- Left True
--
-- TODO: quickcheck properties for typeclass laws
--
instance Applicative (Either a) where
  pure = Right
  Left e  <*> _ = Left e
  Right f <*> e = fmap f e

-- | @Applicative@ instance for @(a, )@.
--
-- >>> pure 1 :: (Any, Int)
-- (Any {getAny = False},1)
--
-- >>> (Sum 1, (+ 1)) <*> (Sum 2, 3)
-- (Sum {getSum = 3},4)
--
-- >>> (Any True, (+ 1)) <*> (Any False, 2)
-- (Any {getAny = True},3)
--
-- TODO: quickcheck properties for typeclass laws
--
instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (x, f) <*> (y, z) = (x `mappend` y, f z)

-- | @Applicative@ instance for @a -> @.
--
-- >>> pure 1 True
-- 1
--
-- >>> (+) <*> (* 2) $ 1
-- 3
--
-- TODO: quickcheck properties for typeclass laws
--
instance Applicative ((->) a) where
  pure = const
  f <*> g = \x -> f x (g x)

-- | @Applicative@ instance for @[]@.
--
-- >>> pure 1 :: [Int]
-- [1]
--
-- >>> [(+ 1), (* 2)] <*> [1 .. 3]
-- [2,3,4,2,4,6]
--
-- TODO: quickcheck properties for typeclass laws
--
instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Ord, Read, Show)

-- | @Applicative@ instance for @ZipList@.
--
-- >>> take 3 . getZipList $ pure 1
-- [1,1,1]
--
-- >>> getZipList $ ZipList [(+ 1), (* 2)] <*> ZipList [1 .. 3]
-- [2,4]
--
-- TODO: quickcheck properties for typeclass laws
--
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

-- | @Monad@ instance for @Maybe@.
--
-- >>> Just 1 >>= \ x -> Just (x + 2)
-- Just 3
--
-- >>> Nothing >>= \ x -> Just (x + 2)
-- Nothing
--
-- >>> return 1 :: Maybe Int
-- Just 1
--
-- TODO: quickcheck properties for typeclass laws
--
instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x  >>= f = f x

-- | @Monad@ instance for @[]@.
--
-- >>> [1 .. 3] >>= \ x -> [x, x]
-- [1,1,2,2,3,3]
--
-- >>> [] >>= \ x -> [x, x]
-- []
--
-- >>> return 1 :: [Int]
-- [1]
--
-- TODO: quickcheck properties for typeclass laws
--
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

-- | @MonadPlus@ instance for @Maybe@.
--
-- >>> Just 1 `mplus` Just 2
-- Just 1
--
-- >>> mzero :: Maybe a
-- Nothing
--
-- TODO: quickcheck properties for typeclass laws
--
instance MonadPlus Maybe where
  mzero = Nothing
  Just x  `mplus` _ = Just x
  Nothing `mplus` m = m

-- | @MonadPlus@ instance for @[]@.
--
-- >>> [1, 2] `mplus` [3, 4]
-- [1,2,3,4]
--
-- >>> mzero :: [a]
-- []
--
-- TODO: quickcheck properties for typeclass laws
--
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
data MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- | @MonadTrans@ instance for @MaybeT@.
--
-- TODO: doctests
--
-- TODO: quickcheck properties for typeclass laws
--
instance MonadTrans MaybeT where
  lift m = MaybeT (m >>= return . Just)

-- | @Monad@ instance for @MaybeT m@.
--
-- TODO: doctests
--
-- TODO: quickcheck properties for typeclass laws
--
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

-- | @MonadPlus@ instance for @MaybeT m@.
--
-- TODO: doctests
--
-- TODO: quickcheck properties for typeclass laws
--
instance Monad m => MonadPlus (MaybeT m) where
  mzero = MaybeT (return Nothing)
  x `mplus` y =
    MaybeT $ runMaybeT x >>= \ m -> case m of
                                      Nothing -> return m
                                      Just _  -> runMaybeT y

-- TODO: add primatives / functions from Control.Monad.Trans.Maybe
