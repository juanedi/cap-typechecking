module TestUtil where

import Test.HUnit

import Util
import Type
import Type.MUType
import Type.Equivalence
import Type.Subtyping
import Term
import Term.Compatibility as C
import Fallible
import Typechecker
import Data.Map

{- Equality matchers -}

(~~) :: Type -> Type -> Assertion
(~~) t1 t2 = (@?) (typeEq t1 t2) $ (show t1) ++ " should be considered equal to " ++ (show t2)

(/~) :: Type -> Type -> Assertion
(/~) t1 t2 = (@?) (not $ typeEq t1 t2) $ (show t1) ++ " should not be considered equal to " ++ (show t2)


(~~~) :: MUType -> MUType -> Assertion
(~~~) t1 t2 = (@?) (muTypeEq t1 t2) $ (show t1) ++ " should be considered equal to " ++ (show t2)

(/~~) :: MUType -> MUType -> Assertion
(/~~) t1 t2 = (@?) (not $ muTypeEq t1 t2) $ (show t1) ++ " should not be considered equal to " ++ (show t2)


{- Subtyping matchers -}

(<~) :: Type -> Type -> Assertion
(<~) t1 t2 = (@?) (subtype t1 t2) $ (show t1) ++ " should be considered a subtype of " ++ (show t2)

(</~) :: Type -> Type -> Assertion
(</~) t1 t2 = (@?) (not $ subtype t1 t2) $ (show t1) ++ " should not be considered a subtype of " ++ (show t2)


{- Compatibility matchers -}

(@~) :: Assignment -> Assignment -> Assertion
(@~) j1 j2 = (@?) (C.compatible j1 j2) $ (show j1) ++ " should be compatible with " ++ (show j2)

(@/~) :: Assignment -> Assignment -> Assertion
(@/~) j1 j2 = (@?) (not $ C.compatible j1 j2) $ (show j1) ++ " should not be compatible with " ++ (show j2)


{- Typechecking matchers -}

ctx :: [(Label,Type)] -> Ctx
ctx = Data.Map.fromList

(|/-) :: [(Label,Type)] -> Term -> Assertion
(|/-) bindings term = let
                        ret = typeOf (ctx bindings) term
                      in
                      (@?) (isError ret) $ "Type error was expected but got result " ++ (show (getValue ret))


(||-) :: [(Label,Type)] -> Term -> Assertion
(||-) bindings term = let
                       c     = ctx bindings
                       check = isSuccess (typeOf c term)
                     in
                       (@?) check $ (show term) ++ " should be typeable under context " ++ (show c)

(|-) :: [(Label,Type)] -> Term -> (Ctx, Term)
(|-) bindings term = (ctx bindings,term)

(|:) :: (Ctx, Term) -> Type -> Assertion
(|:) (c, term) t = (runFallible $ typeOf c term) ~~ t