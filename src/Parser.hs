module Parser (
    Parser (..)
  , runParser
  , zero

  , item
  , sat
  , char
  , digit
  , lower
  , upper
  ) where

import Control.Applicative ( Alternative (..) )
import Control.Monad (
    MonadPlus (..)
  , ap
  , liftM
  )
import Data.Char (
    isDigit
  , isLower
  , isUpper
  )

-- | Data type for parser. The first type parameter represents the input to the
-- parser, e.g., 'String'. The second type parameter represents the parsed
-- value.
--
-- A parser is a function that takes input and yields a list of pairs of a value
-- and an unconsumed suffix of the input. An empty list indicates failure to
-- parse the input. A list with multiple elements indicates the input can be
-- parsed in multiple ways.
data Parser s a = P (s -> [(a, s)])

runParser :: Parser s a -> s -> [(a, s)]
runParser (P p) = p

instance Monad (Parser s) where
  return x = P $ \ s -> [(x, s)]
  P p >>= f = P $ \ s -> concat [runParser (f x) s' | (x, s') <- p s]

-- Boilerplate
instance Applicative (Parser s) where
  pure = return
  (<*>) = ap

-- Boilerplate
instance Functor (Parser s) where
  fmap = liftM

instance MonadPlus (Parser s)

instance Alternative (Parser s) where
  empty = zero
  P p <|> P q = P $ \ s -> p s ++ q s

-- | A parser that fails on any input.
zero :: Parser s a
zero = P (const [])

-- | A parser that consumes the first character of an input string if non-empty,
-- otherwise fails.
item :: Parser String Char
item = P $ \ s -> case s of
                    []     -> []
                    (x:xs) -> [(x,xs)]

-- | A parser combinator that takes a predicate and returns a parser that
-- consumes a character if it satisfies the predicate.
sat :: (Char -> Bool) -> Parser String Char
sat p = do { x <- item ; if p x then return x else zero }

-- | A parser combinator that takes a character and returns a parser that
-- consumes that character.
char :: Char -> Parser String Char
char x = sat (x ==)

digit :: Parser String Char
digit = sat isDigit

lower :: Parser String Char
lower = sat isLower

upper :: Parser String Char
upper = sat isUpper
