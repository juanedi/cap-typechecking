import Test.HUnit
import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)

import TestUtil

import Util
import Type
import Term

main :: IO ()
main = hspec $ do fromHUnitTest (allTests)

allTests :: Test
allTests = let
    t1 = TVar 1 T
    t2 = TVar 2 T
  in
  test [
  (PConst "c", t1) @~ (PConst "c", t1),
  (PConst "c", t1) @~ (PConst "d", t2),
  
  (PConst "c", t1) @/~ (PConst "c", t2),
  (PMatch "y", t2) @/~ (PConst "c", t1),

  (PConst "c", TRec $ t1 `TUnion` t1) @~ (PConst "c", TRec $ t1 `TUnion` (t1 `TUnion` t1))
  ]