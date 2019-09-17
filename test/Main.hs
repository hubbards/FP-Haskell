import Test.DocTest ( doctest )

main :: IO ()
main = doctest . map ("src/" ++) $
  [ "Tree.hs"
  , "Calculator.hs" ]
