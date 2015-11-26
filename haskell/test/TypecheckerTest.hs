import Test.HUnit
import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)

import TestUtil

import Util
import Term
import Type

main :: IO ()
main = hspec $ do fromHUnitTest (allTests)

allTests :: Test
allTests = test [atomTests, caseTests, applicationTests]

atomTests :: Test
atomTests = test [
    "{} |- _c : c"     ~:
    []  |-  Const "c"  |:  TConst "c",

    "{a : c} |- a : c" ~:
    [("a", TConst "c")]  |-  Var "a"  |:  TConst "c",

    "{} |/- a"         ~:
    []  |/-  Var "a"
  ]

caseTests :: Test
caseTests = test [
    "{} |- (_c -> _d) :  c > d" ~:
    []  |-  Case [(PConst "c", ctx [], Const "d")]  |: TFunc (TConst "c") (TConst "d"),

    "{} |/- (_c -> a)" ~:
    []  |/-  Case [(PConst "c", ctx [], Var "a")],

    "{a : 1} |- (_c -> a) :  c > 1" ~:
    [("a", TVar 1 T)]  |-  Case [(PConst "c", ctx [], Var "a")]  |: TFunc (TConst "c") (TVar 1 T),

    "{} |- (a {a:1}-> _d) : 1 > d" ~:
    []  |-  Case [(PMatch "a", ctx [("a", TVar 1 T)], Const "d")]  |: TFunc (TVar 1 T) (TConst "d"),

    "{} |- (a {a:1}-> a)  : 1 > 1" ~:
    []  |-  Case [(PMatch "a", ctx [("a", TVar 1 T)], Var "a")]  |: TFunc (TVar 1 T) (TVar 1 T),

    -- invalid binding context for branch
    "{} |/- (_c {a:1}-> _c)" ~:
    []  |/-  Case [(PConst "c", ctx [("a", TVar 1 T)], Const "c")],

    -- invalid binding context for branch
    "{} |/- (a {}-> _c)" ~:
    []  |/-  Case [(PMatch "a", ctx [], Const "c")],


    "{} |- (_c -> _e | _d -> _e)   : (c + d) > e" ~:
    let
      branch1 = (PConst "c", ctx [], Const "e")
      branch2 = (PConst "d", ctx [], Const "e")
    in
      [] |- Case [branch1, branch2] |: TFunc (TConst "c" `TUnion` TConst "d") (TConst "e"),

    -- overlapping incompatible branches
    "{} |/- (_c x {x:1}-> _e | _c y {y:2}-> _e)" ~:
    let
      branch1 = (PComp (PConst "c") (PMatch "x"), ctx [("x", TVar 1 T)], Const "e")
      branch2 = (PComp (PConst "c") (PMatch "y"), ctx [("y", TVar 2 T)], Const "e")
    in
      [] |/- Case [branch1, branch2],
    
    -- compatibility is not checked with previous branches
    "{} |/- (_c {}-> _e | y {y:1}-> _e)" ~:
    let
      dv1 = TVar 1 D
      c = TConst "c"
      d = TConst "d"
      e = TConst "e"
      branch1 = (PConst "c", ctx [], Const "d")
      branch2 = (PMatch "y", ctx [("y", dv1)], Const "e")
    in
      [] |- Case [branch1, branch2] |: TFunc (TUnion dv1 c) (TUnion d e),


    "{} |- (_c -> _e | _d -> _f)  : (c + d) > (e + f)" ~:
    let
      branch1 = (PConst "c", ctx [], Const "e")
      branch2 = (PConst "d", ctx [], Const "f")
    in
      [] |- Case [branch1, branch2] |: TFunc (TConst "c" `TUnion` TConst "d") (TConst "e" `TUnion` TConst "f")
  ]


applicationTests :: Test
applicationTests = test [
    "{a : 1} |- _c a : c @ a" ~:
    [("a", TVar 1 T)]   |-   App (Const "c") (Var "a")  |:  TComp (TConst "c") (TVar 1 T),

    "{} |- (_c -> _d) _c :  d" ~:
    []  |-  App (Case [(PConst "c", ctx [], Const "d")]) (Const "c")  |: TConst "d",

    -- argument type not compatible with single branch
    "{} |/- (_c -> _d) _e" ~:
    []  |/-  App (Case [(PConst "c", ctx [], Const "d")]) (Const "e"),

    -- argument type not compatible with one branch
    "{} |/- (_c -> _e | _d -> _e) _c" ~:
    let
      branch1 = (PConst "c", ctx [], Const "e")
      branch2 = (PConst "d", ctx [], Const "e")
    in
    []  |/-  App (Case [branch1, branch2]) (Const "f")
  ]