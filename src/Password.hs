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
import Data.Char (
    isDigit
  , isLetter
  )

-- NOTE: from transformers package
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Maybe ( MaybeT )

-- | Prompt user for password.
getPassword :: IO String
getPassword = do
  putStr "Enter password: " ; hFlush stdout
  getLine

-- | "Saves" password.
--
-- NOTE: In a real application this would hash and salt the application before
-- storing it in a database.
--
setPassword :: String -> IO ()
setPassword _ = putStrLn "Saving password."

-- | Prompt user for new password then save it to file.
newPassword :: IO ()
newPassword = getPassword >>= setPassword

-- | Check if password is legal.
isLegal :: String -> Bool
isLegal p = n > 5 && n < 10 && all (\ c -> isDigit c || isLetter c) p where
  n = length p

-- | Prompt user for legal password; @Nothing@ if password is illegal.
getLegalPassword :: MaybeT IO String
getLegalPassword = do
  p <- lift getPassword
  -- failure is propagated
  guard (isLegal p) ; return p

-- | Password was illegal, try again.
tryAgain :: MaybeT IO String
tryAgain = lift (putStrLn "Illegal password, try again.") >> getLegalPassword

-- | Prompt user for password until provided a legal password then save it to
-- file.
newLegalPassword :: MaybeT IO ()
newLegalPassword = do
  -- return first Just value
  p <- msum (getLegalPassword : repeat tryAgain)
  lift (setPassword p)
