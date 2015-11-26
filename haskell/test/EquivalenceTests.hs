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
    v n  = TVar n T
    dv n = TVar n D

    v1 = v 1
    v2 = v 2
    v3 = v 3
  in
  test [
  "1 ~~ 1" ~: v1 ~~ v1,
  "1 /~ 1" ~: v1 /~ v2,

  "1 > 2 ~~ 1 > 2" ~:
  (v1 `TFunc` v2) ~~ (v1 `TFunc` v2),

  "1 > 2 /~ 2 > 2" ~:
  (v1 `TFunc` v2) /~ (v2 `TFunc` v2),

  "1 > 2 /~ 1 > 1" ~:
  (v1 `TFunc` v2) /~ (v1 `TFunc` v1),

  "1 > 2 /~ 1 > 1" ~:
  ((dv 1) `TComp` v2) ~~ ((dv 1) `TComp` v2),

  "1 > 2 /~ 1 > 1" ~:
  (v1 `TComp` v2) /~ (v1 `TComp` v2),

  "1 ~~ 1 (+) 1" ~:
  v1 ~~ (v1 `TUnion` v1),

  "1 (+) 1 ~~ 1" ~:
  (v1 `TUnion` v1) ~~ v1,

  "1 (+) 1 ~~ 1 (+) 1 (+) 1" ~:
  (v1 `TUnion` v1) ~~ (v1 `TUnion` v1 `TUnion` v1),

  "1 ~~ 1 (+) 1 (+) 1" ~:
  v1 ~~ (v1 `TUnion` v1 `TUnion` v1),

  "1 (+) 2 ~~ 1 (+) 2 (+) 1" ~:
  (v1 `TUnion` v2) ~~ (v1 `TUnion` v2 `TUnion` v1),

  "1 (+) 2 ~~ 2 (+) 1" ~:
  (v1 `TUnion` v2) ~~ (v2 `TUnion` v1),

  "1 (+) 2 ~~ 1 (+) 2" ~:
  (v1 `TUnion` v2) ~~ (v1 `TUnion` v2),
  
  "1 (+) 2 /~ 1 (+) 1" ~:
  (v1 `TUnion` v2) /~ (v1 `TUnion` v1),
  
  "1 (+) (2 (+) 3) ~~ (1 (+) 2) (+) 3" ~:
  (v1 `TUnion` (v2 `TUnion` v3)) ~~ ((v1 `TUnion` v2) `TUnion` v3),

  "µ. 1 > 2 ~~ µ. 1 > 2 " ~:
  (TRec $ v1 `TFunc` v2) ~~ (TRec $ v1 `TFunc` v2),

  "µ. 1 > 2 ~~ (µ. 1 > 2) > 1 " ~:
  (TRec $ v1 `TFunc` v2) ~~ ((TRec $ v1 `TFunc` v2) `TFunc` v1),

  "µ. 1 > 2 /~ µ. 1 > 3" ~:
  (TRec $ v1 `TFunc` v2) /~ (TRec $ v1 `TFunc` v3),

  "µ. 10 > (µ. 1 > 2) ~~ 9 > (µ. 1 > (µ. 11 > (µ. 1 > 2)))" ~:
  (TRec $ (v 10) `TFunc` (TRec $ v1 `TFunc` v2)) ~~ ((v 9) `TFunc` (TRec $ v1 `TFunc` (TRec $ (v 11) `TFunc` (TRec $ v1 `TFunc` v2)))),

  "a (+) (µ. ((1 > b) (+) c)) ~~ a (+) ((µ. ((1 > b) (+) c)) > b) (+) c" ~:
  let
    a = TConst "a"
    b = TConst "b"
    c = TConst "c"
    t = TRec $ (v1 `TFunc` b) `TUnion` c
  in
    (a `TUnion` t) ~~ (a `TUnion` (t `TFunc` b) `TUnion` c)
  ]