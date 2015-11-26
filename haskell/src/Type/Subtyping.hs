module Type.Subtyping where

import Util
import Type
import Type.MUType as MUType
import Type.Equivalence
import Fallible

subtype :: Type -> Type -> Bool
subtype a b = muSubtype (toMUType a) (toMUType b)

muSubtype :: MUType -> MUType -> Bool
muSubtype a b = isSuccess (muSubtype' a b [])

muSubtype' :: MUType -> MUType -> AssumptionSet -> Result AssumptionSet
muSubtype' a b s = if (a,b) `elem` s
                   then
                     return s
                   else
                     let s0 = (a,b):s in
                     case (a,b) of
                       (MUTVar v1 _, MUTVar v2 _) | v1 == v2 ->
                         return s0

                       (MUTConst l1, MUTConst l2) | l1 == l2 ->
                         return s0

                       (MUTComp a1 a2, MUTComp b1 b2) | isDataType a1 && isDataType b1 -> do
                         s1 <- muSubtype' a1 b1 s0
                         muSubtype' a2 b2 s1
                       
                       (MUTFunc a1 a2, MUTFunc b1 b2) -> do
                         s1 <- muSubtype' b1 a1 s0
                         muSubtype' a2 b2 s1

                       (MUTRec _, _) ->
                         muSubtype' (MUType.unfold a) b s0

                       (_, MUTRec _) ->
                         muSubtype' a (MUType.unfold b) s0

                       (MUTUnion as, _) -> do
                         Fallible.fold (\s' a' -> muSubtype' a' b s') (return s0) as

                       (_, MUTUnion bs) ->
                         Fallible.seq $ Prelude.map (\b' -> muSubtype' a b' s0) bs

                       _ -> failure undefined