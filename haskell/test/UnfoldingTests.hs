import Test.HUnit
import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)

import Util
import Type.MUType

main :: IO ()
main = hspec $ do fromHUnitTest (allTests)

allTests :: Test
allTests = let
    v n = MUTVar n T
    v1 = v 1
    v2 = v 2
    v3 = v 3
  in
  test [
    "decrease free variables to account for removed binder" ~:
    -- µ. (1 > 2)
    -- ~~> (µ. (1 > 2)) > 1
    ((MUTRec $ v1 `MUTFunc` v2) `MUTFunc` v1) ~=? (unfold $ MUTRec $ v1 `MUTFunc` v2),

    -- µ. (1 > 3)
    -- ~~> (µ. (1 > 3)) > 2
    ((MUTRec $ v1 `MUTFunc` v3) `MUTFunc` v2) ~=? (unfold $ MUTRec $ v1 `MUTFunc` v3),

    "increase free variables in body when replacing under nested binders" ~:
    -- µ. ((µ. (2 > 10)) > 2)
    -- ~~> (µ. ((µ. ((µ. (2 > 11)) > 2)) > 9)) > 1
    ((MUTRec $ (MUTRec $ (MUTRec $ v2 `MUTFunc` (v 11)) `MUTFunc` v2) `MUTFunc` (v 9)) `MUTFunc` v1) ~=? (unfold $ MUTRec $ (MUTRec $ v2 `MUTFunc` (v 10)) `MUTFunc` v2),

    "leave variables bound by other binders untouched" ~:
    -- µ. ((µ. (1 > 10)) > 2)
    -- ~~> (µ. (1 > 9)) > 1
    ((MUTRec $ v1 `MUTFunc` (v 9)) `MUTFunc` v1) ~=? (unfold $ MUTRec $ (MUTRec $ v1 `MUTFunc` (v 10)) `MUTFunc` v2)
  ]