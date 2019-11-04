module RegisterTest ( tests ) where

-- NOTE: from HUnit package
import Test.HUnit (
    Test
  , (~:)
  , (@=?)
  )

import Register

tests :: Test
tests = "Register" ~:
  [ "state" ~:
    [ "set1" ~: (3, 2) @=? set1 3 (1, 2)
    , "set2" ~: (1, 3) @=? set2 3 (1, 2) ]
  , "eval" ~:
    [ (1, (-1, 0)) @=? eval' (Add (Lit 2) (Save1 $ Neg $ Lit 1))
    , (3, (1, 2))  @=? eval' (Add (Save1 $ Lit 1) (Save2 $ Lit 2))
    , (4, (2, 2))  @=? eval' (Mul (Save1 $ Lit 2) (Save2 Load1))
    , (3, (1, 2))  @=? eval' (SetIn1 1 (SetIn2 2 $ Add Load1 Load2))
    , (3, (2, 2))  @=? eval' (SetIn1 1 (SetIn2 2 $ Add Load1 $ Save1 Load2)) ] ]
