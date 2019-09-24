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
main = runTestTT . test $
  [ "Monad"    ~: MonadTest.tests
  , "Register" ~: RegisterTest.tests
  , "Stack"    ~: StackTest.tests
  , "Tree"     ~: TreeTest.tests ]
