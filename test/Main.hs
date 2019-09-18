import Test.DocTest ( doctest )

main :: IO ()
main = doctest . map ("src/" ++) $
  [ "Calculator.hs"
  , "Monad.hs"
  , "Tree.hs" ]
