module Term.Compatibility(compatible) where

import Util
import Term
import Type
import Type.Subtyping

import Data.Set

data Symbol = MSTVar Index
            | MSConst Label
            | MSAbs
            | MSApp
            deriving (Eq, Ord, Show)


compatible :: Assignment -> Assignment -> Bool
compatible a1@(p, a) a2@(q, b) = not (pcomp a1 a2) || subtype b a

pcomp :: Assignment -> Assignment -> Bool
pcomp (PComp p1 p2, TComp a1 a2) (PComp q1 q2, TComp b1 b2) = pcomp (p1,a1) (q1,b1) && pcomp (p2,a2) (q2,b2)
pcomp (p,a) (q,b) = case (p,q) of
                      (PMatch _,_)                  -> True
                      (PConst c, PConst d) | c == d -> True
                      _                             -> let
                                                         s1 = admittedSymbols a
                                                         s2 = admittedSymbols b
                                                       in
                                                         commonSymbol s1 s2


admittedSymbols :: Type -> Set Symbol
admittedSymbols (TVar v _)   = singleton (MSTVar v)
admittedSymbols (TConst l)   = singleton (MSConst l)
admittedSymbols (TComp _ _)  = singleton MSApp
admittedSymbols (TFunc _ _)  = singleton MSAbs
admittedSymbols (TUnion a b) = union (admittedSymbols a) (admittedSymbols b)
admittedSymbols (TRec body)  = Data.Set.map decrementVar (admittedSymbols body)
                               where decrementVar s = case s of
                                                        (MSTVar n) -> MSTVar (n-1)
                                                        _          -> s

commonSymbol :: Set Symbol -> Set Symbol -> Bool
commonSymbol s1 s2 = not.(Data.Set.null) $ intersection s1 s2