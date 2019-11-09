module RegisterTest ( tests ) where

-- NOTE: from HUnit package
import Test.HUnit (
    Test
  , (~:)
  , (@=?)
  )

import Register

tests :: Test
tests = "Register" ~: [ testEval ]

testEval :: Test
testEval = "eval" ~: [
    (1, (-1, 0)) @=? eval' (Add (Lit 2) (Save1 $ Neg $ Lit 1))
  , (3, (1, 2))  @=? eval' (Add (Save1 $ Lit 1) (Save2 $ Lit 2))
  , (4, (2, 2))  @=? eval' (Mul (Save1 $ Lit 2) (Save2 Load1))
  , (3, (1, 2))  @=? eval' (SetIn1 1 (SetIn2 2 $ Add Load1 Load2))
  , (3, (2, 2))  @=? eval' (SetIn1 1 (SetIn2 2 $ Add Load1 $ Save1 Load2))
  ]
