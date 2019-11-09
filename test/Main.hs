-- NOTE: from filepath package
import System.FilePath ( (</>) )
-- NOTE: from doctest package
import Test.DocTest ( doctest )
-- NOTE: from HUnit package
import Test.HUnit (
    Counts
  , runTestTT
  , test
  , (~:)
  )

import qualified MonadTest
import qualified QueueTest
import qualified RedBlackTest
import qualified RegisterTest
import qualified SetTest
import qualified StackTest
import qualified TreeTest


main :: IO Counts
main = do
  doctest ["src" </> "Tree.hs"]
  runTestTT . test $
    [ MonadTest.tests
    , QueueTest.tests
    , RedBlackTest.tests
    , RegisterTest.tests
    , SetTest.tests
    , StackTest.tests
    , TreeTest.tests ]
