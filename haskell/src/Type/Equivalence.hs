module Type.Equivalence where

import Util
import Type
import Type.MUType as MUType
import Fallible
import Data.List
import Data.Maybe

typeEq :: Type -> Type -> Bool
typeEq t1 t2 = muTypeEq (toMUType t1) (toMUType t2)

muTypeEq :: MUType -> MUType -> Bool
muTypeEq t1 t2 = isSuccess $ muTypeEq' t1 t2 []

muTypeEq' :: MUType -> MUType -> AssumptionSet -> (Result AssumptionSet)
muTypeEq' a b s = if (a,b) `elem` s
                  then
                    return s
                  else
                    let s0 = (a,b):s in
                    case (a,b) of
                      (MUTVar l1 _ , MUTVar l2 _) | l1 == l2 -> return s0
                      (MUTConst l1, MUTConst l2)  | l1 == l2 -> return s0

                      (MUTComp a1 a2, MUTComp b1 b2) | isDataType a1 && isDataType b1 -> do
                        s1 <- muTypeEq' a1 b1 s0
                        muTypeEq' a2 b2 s1
                      
                      (MUTFunc a1 a2, MUTFunc b1 b2) -> do
                        s1 <- muTypeEq' a1 b1 s0
                        muTypeEq' a2 b2 s1

                      _ | hasRec a ->
                        muTypeEq' (unfoldRec a) b s0

                      _ | hasRec b ->
                        muTypeEq' a (unfoldRec b) s0

                      -- A may or may not be a union
                      (_, MUTUnion bs) ->
                        let
                          as = MUType.toList a
                          sn = fold (eqSeq bs) (return s0) as
                          sf = fold (eqSeq as) sn bs
                        in sf

                      -- B is not a union but A is
                      (MUTUnion as, _) -> do
                        sn <- fold (eqSeq [b]) (return s0) as
                        eqSeq as sn b

                      _ -> failure undefined


eqSeq :: [MUType] -> AssumptionSet -> MUType -> Result AssumptionSet
eqSeq ts s pivot = Fallible.seq [ muTypeEq' pivot t s | t <- ts]

hasRec :: MUType -> Bool
hasRec = isJust.findRec

findRec :: MUType -> Maybe Int
findRec t@(MUTRec _)  = Just 0
findRec (MUTUnion as) = findIndex isRec as
findRec _             = Nothing

unfoldRec :: MUType -> MUType
unfoldRec t@(MUTRec _)    = MUType.unfold t
unfoldRec t@(MUTUnion as) = let
                               i = fromJust (findRec t)
                               (l1, h:l2) = splitAt i as
                             in
                               flatUnion $ l1 ++ [MUType.unfold h] ++ l2

isRec :: MUType -> Bool
isRec (MUTRec _) = True
isRec _          = False

isPrefix :: MUType -> Bool
isPrefix (MUTUnion _) = False
isPrefix (MUTRec _)   = False
isPrefix _            = True
