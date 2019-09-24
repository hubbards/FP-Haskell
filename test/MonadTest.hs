module MonadTest (
    tests
  , prop_functorIdentity
  , prop_mapFusion
  , prop_applicativeIdentity
--  , prop_homomorphism
  , prop_composition
  , prop_application
  , prop_applicativeFunctor
  ) where

import Test.HUnit (
    Test
  , (~:)
  , (@=?)
  )
import Data.Monoid (
    Sum (..)
  , Any (..)
  )
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
import Monad

prop_functorIdentity :: (Functor t, Eq (t a)) => t a -> Bool
prop_functorIdentity x = fmap id x == id x

prop_mapFusion :: (Functor t, Eq (t c)) => (b -> c) -> (a -> b) -> t a -> Bool
prop_mapFusion g f x = fmap (g . f) x == (fmap g . fmap f) x

prop_applicativeIdentity :: (Applicative t, Eq (t a)) => t a -> Bool
prop_applicativeIdentity tx = pure id <*> tx == tx

-- FIXME: ambiguous type
-- prop_homomorphism :: (Applicative t, Eq (t b)) => (a -> b) -> a -> Bool
-- prop_homomorphism f x = pure f <*> pure x == pure (f x)

prop_composition :: (Applicative t, Eq (t c)) =>
                    t (b -> c) -> t (a -> b) -> t a -> Bool
prop_composition tu tv tw = pure (.) <*> tu <*> tv <*> tw == tu <*> (tv <*> tw)

prop_application :: (Applicative t, Eq (t b)) => t (a -> b) -> a -> Bool
prop_application tf x = tf <*> pure x == pure ($ x) <*> tf

prop_applicativeFunctor :: (Applicative t, Eq (t b)) => (a -> b) -> t a -> Bool
prop_applicativeFunctor f tx = fmap f tx == pure f <*> tx

-- TODO: add properties

tests :: [Test]
tests =
  [ "Functor" ~:
    [ "Maybe" ~:
      [ Just 3  @=? fmap (+ 1) (Just 2)
      , Nothing @=? fmap (+ 1) Nothing ]
    , "Either a" ~:
      [ Right 3   @=? fmap (+ 1) (Right 2 :: Either Bool Int)
      , Left 2    @=? fmap (+ 1) (Left 2)
      , Left True @=? fmap (+ 1) (Left True) ]
    , "(a, )" ~:
      [ (2, 4)    @=? fmap (+ 1) (2, 3)
      , (True, 3) @=? fmap (+ 1) (True, 2) ]
    , "a -> " ~:
      [ 7     @=? fmap (+ 1) (* 2) 3
      , False @=? fmap even (+ 1) 2 ]
    , "[]" ~:
      [ [2, 3, 4] @=? fmap (+ 1) [1 .. 3]
      , []        @=? fmap (+ 1) [] ] ]

  , "Applicative" ~:
    [ "Maybe" ~:
      [ Just 1  @=? (pure 1 :: Maybe Int)
      , Just 3  @=? Just (+ 1) <*> Just 2
      , Nothing @=? Just (+ 1) <*> Nothing
      , Nothing @=? (Nothing :: Maybe (Int -> Int)) <*> Just 2
      , Nothing @=? (Nothing :: Maybe (Int -> Int)) <*> Nothing ]
    , "Either a" ~:
      [ Right 1   @=? (pure 1 :: Either Bool Int)
      , Right 3   @=? Right (+ 1) <*> (Right 2 :: Either Bool Int)
      , Left 2    @=? Right (+ 1) <*> Left 2
      , Left True @=? Right (+ 1) <*> Left True
      , Left True @=? (Left True :: Either Bool (Int -> Int)) <*> Right 2 ]
    , "(a, )" ~:
      [ (Any False, 1) @=? (pure 1 :: (Any, Int))
      , (Sum 3, 4)     @=? (Sum 1, (+ 1)) <*> (Sum 2, 3)
      , (Any True, 3)  @=? (Any True, (+ 1)) <*> (Any False, 2) ]
    , "a -> " ~:
      [ 1 @=? (pure 1 :: Bool -> Int) True
      , 3 @=? ((+) <*> (* 2)) 1 ]
    , "[]" ~:
      [ [1]                @=? (pure 1 :: [Int])
      , [2, 3, 4, 2, 4, 6] @=? [(+ 1), (* 2)] <*> [1 .. 3] ]
    , "ZipList" ~:
      [ [1, 1, 1]      @=? (take 3 . getZipList . pure) 1
      , ZipList [2, 4] @=? ZipList [(+ 1), (* 2)] <*> ZipList [1 .. 3] ] ]

  , "Monad" ~:
    [ "Maybe" ~:
      [ Just 1  @=? (return 1 :: Maybe Int)
      , Just 3  @=? (Just 1 >>= Just . (+ 2))
      , Nothing @=? (Nothing >>= Just . (+ 2)) ]
    , "[]" ~:
      [ [1]                @=? (return 1 :: [Int])
      , [1, 1, 2, 2, 3, 3] @=? ([1 .. 3] >>= replicate 2)
      , ([] :: [Int])      @=? ([] >>= replicate 2) ]
    , "MaybeT" ~:
      [ [Just 1]                   @=? runMaybeT (return 1 :: MaybeT [] Int)
      , [Just 1, Just 1]           @=?
        runMaybeT (MT [Just 1] >>= MT . replicate 2 . Just)
      , ([Nothing] :: [Maybe Int]) @=?
        runMaybeT (MT [Nothing] >>= MT . replicate 2 . Just) ] ]

  , "MonadPlus" ~:
    [ "Maybe" ~:
      [ Nothing @=? (mzero :: Maybe Int)
      , Just 1  @=? Just 1 `mplus` Just 2 ]
    , "[]" ~:
      [ []           @=? (mzero :: [Int])
      , [1, 2, 3, 4] @=? [1, 2] `mplus` [3, 4] ]
    , "MaybeT" ~:
      [ [Nothing] @=? runMaybeT (mzero :: MaybeT [] Int)
      , [Just 1]  @=? runMaybeT (MT [Just 1] `mplus` MT [Just 2]) ] ]

  , "MonadTrans" ~:
    [ "MaybeT" ~:
      [ [Just 1, Just 2, Just 3] @=? runMaybeT (lift [1 .. 3]) ] ] ]
