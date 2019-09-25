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
import qualified RegisterTest
import qualified StackTest
import qualified TreeTest

main :: IO Counts
main = do
  doctest ["src" </> "Tree.hs"]
  runTestTT . test $
    [ "Monad"    ~: MonadTest.tests
    , "Register" ~: RegisterTest.tests
    , "Stack"    ~: StackTest.tests
    , "Tree"     ~: TreeTest.tests ]
