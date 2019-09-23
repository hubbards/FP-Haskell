-- | This module contains a simple password API using monad transformers to
-- layer effects.
--
-- NOTE: this is NOT intended to demonstrate any security best practices
--
module Password where

import System.IO (
    hFlush
  , stdout
  )
import Control.Monad (
    guard
  , msum
  )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Maybe ( MaybeT )
import Data.Char (
    isDigit
  , isLetter
  )

-- | Prompt user for password.
getPassword :: IO String
getPassword = do
  putStr "Enter password: " ; hFlush stdout
  getLine

-- | Saves password to file.
setPassword :: String -> IO ()
setPassword = writeFile "password.txt"

-- | Prompt user for new password then save it to file.
newPassword :: IO ()
newPassword = getPassword >>= setPassword

-- | Check if password is legal.
isLegal :: String -> Bool
isLegal p = n > 5 && n < 10 && all (\ c -> isDigit c || isLetter c) p where
  n = length p

-- | Prompt user for legal password; @Nothing@ if password is illegal.
--
-- NOTE: @lift@ is used to lift a computation from @IO@ to @MaybeT@
--
-- NOTE: for @MaybeT@, @guard False@ returns @Nothing@
--
-- NOTE: for @MaybeT@, @Nothing@ is propagated by @(>>=)@
--
getLegalPassword :: MaybeT IO String
getLegalPassword = do
  p <- lift getPassword
  guard (isLegal p) ; return p

-- | Password was illegal, try again.
tryAgain :: MaybeT IO String
tryAgain = lift (putStrLn "Illegal password, try again.") >> getLegalPassword

-- | Prompt user for password until provided a legal password then save it to
-- file.
--
-- NOTE: for @MaybeT@, @msum@ returns first @Just@ value
--
-- NOTE: @lift@ is used to lift a computation from @IO@ to @Maybe@
--
newLegalPassword :: MaybeT IO ()
newLegalPassword = do
  p <- msum (getLegalPassword : repeat tryAgain)
  lift (setPassword p)
