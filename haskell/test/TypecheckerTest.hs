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
allTests = test [updateTest]

-- build constants
c :: Label -> Type
c = TConst

-- build general type variables
tv :: Index -> Type
tv i = TVar i T

-- build datatype variables
dv :: Index -> Type
dv i = TVar i D

-- build the fix point for a specific function type
fix :: Type -> Term
fix t = Case [(
          PMatch "f",
          ctx [("f", t `TFunc` t)],
          fxx `App` fxx
        )]
        where fxx = Case [(
                      PMatch "x", 
                      ctx [("x", TRec $ tv 1 `TFunc` t)],
                      Var "f" `App` (Var "x" `App` Var "x")
                    )]

updateTest :: Test
updateTest = let
               -- constants
               a = c "a"
               b = c "b"
               
               -- list or tree constructors
               listOrTree = c "nil" `TUnion` c "cons" `TUnion` c "node"

               -- type of the possible arguments of the upd funciton
               d t = TRec $ (c "pt" `TComp` t) `TUnion` (dv 1 `TComp` dv 1) `TUnion` listOrTree

               -- type used to build the fixpoint combinator
               tUpd = (a `TFunc` b) `TFunc` (d a `TFunc` d b)
               
               -- definition of the function, feeded to the fixpoint combinator
               upd = Case [
                       (PMatch "upd", ctx [("upd", tUpd)], Case [
                         (PMatch "f", ctx [("f", a `TFunc` b)], Case [
                           (PConst "pt" `PComp` PMatch "z", ctx [("z", a)], Const "pt" `App` (Var "f" `App` Var "z")),
                           (PMatch "x" `PComp` PMatch "y", ctx [("x", d a), ("y", d a)], (Var "upd" `App` Var "f" `App` Var "x") `App` (Var "upd" `App` Var "f" `App` Var "y")),
                           (PMatch "w", ctx [("w", listOrTree)], Var "w")
                         ])
                       ])
                     ]
             in test [
                []  ||-  App (fix tUpd) upd
               ]