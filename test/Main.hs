import Test.DocTest ( doctest )
import Test.HUnit (
    Counts
  , runTestTT
  )
import MonadTest ( tests )

main :: IO Counts
main = do
  doctest [ "src/Calculator.hs", "src/Tree.hs" ]
  runTestTT tests
