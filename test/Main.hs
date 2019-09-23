import Test.HUnit (
    Counts
  , runTestTT
  , test
  , (~:)
  )
import qualified CalculatorTest
import qualified MonadTest
import qualified TreeTest

main :: IO Counts
main = runTestTT . test $
  [ "Calculator" ~: CalculatorTest.tests
  , "Monad"      ~: MonadTest.tests
  , "Tree"       ~: TreeTest.tests ]
