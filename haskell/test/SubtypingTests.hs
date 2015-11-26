import Test.HUnit
import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)

import TestUtil
import Util
import Type

main :: IO ()
main = hspec $ do fromHUnitTest (allTests)

allTests :: Test
allTests = let
    dv n = TVar n D
    v n  = TVar n T
    a = TConst "a"
    b = TConst "b"
    c = TConst "c"
  in
  test [
  a <~ a,
  (v 1) <~ (v 1),
  (v 1) </~ (v 2),
  a <~ TUnion a b,
  TUnion a b <~ TUnion a b,
  TUnion b a <~ TUnion b a,
  TFunc a b <~ TFunc a b,
  TFunc a b <~ TFunc a (TUnion b c),
  TComp (dv 1) b <~ TComp (dv 1) b,
  TComp (dv 1) b </~ TComp (dv 1) a,
  TComp (v 1) b  </~ TComp (v 1) b,
  TFunc (TUnion a c) b <~ TFunc a b
  ]