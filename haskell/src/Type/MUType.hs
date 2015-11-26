module Type.MUType where

import Util
import qualified Type

type Result = Either String
type AssumptionSet = [(MUType, MUType)]

{-
    Alternative type representation in which unions can contain more
    than two subexpressions.

    This allows to build 'maximal union' types, in which a union and
    all of its immediate child unions can be compressed into the same
    level.

    Invariant for expressions of the form (MUTUnion ts):
      1. ts has at least two elements.
      2. ts doesn't have elements of the form (MUTUnion ts')
-}
data MUType = MUTVar Index Sort
            | MUTConst Label
            | MUTComp MUType MUType
            | MUTFunc MUType MUType
            | MUTUnion [MUType]
            | MUTRec MUType
            deriving Eq

{- fold types -}

foldMUType :: (Index -> Sort -> b) -> (Label -> b) -> (b -> b -> b) -> (b -> b -> b) -> ([b] -> b) -> (b -> b) -> MUType -> b
foldMUType fVar fConst fComp fFunc fUnion fRec t = let rec = foldMUType fVar fConst fComp fFunc fUnion fRec
                                                   in case t of
                                                     MUTVar i s     -> fVar i s
                                                     MUTConst l     -> fConst l
                                                     MUTComp t1 t2  -> fComp  (rec t1) (rec t2)
                                                     MUTFunc t1 t2  -> fFunc  (rec t1) (rec t2)
                                                     MUTUnion ts    -> fUnion (map rec ts)
                                                     MUTRec t'      -> fRec (rec t')

{-
  builds a MUTUnion from a list of types.

  this way of building unions is safer than just using the constructor,
  since it makes sure the type invariant holds. this is, it checks that
  there is at least one type and it flattens the first level of unions.
-}
flatUnion :: [MUType] -> MUType
flatUnion ts | length ts > 1 = MUTUnion $ concatMap toList ts

{-
  the types that make up a union, or a singleton list for non-union types
  useful for treating union and non union types indistinctly
-}
toList :: MUType -> [MUType]
toList (MUTUnion ts) = ts
toList t             = [t]

{-
  If the list contains a single element, return it. Otherwise build a union
  of all elements.

  Being the inverse of toList, this is useful for building a MUType after
  turning it into a list for treating union and non union types indistinctly.
-}
fromList :: [MUType] -> MUType
fromList [t] = t
fromList ts@(_:_) = flatUnion ts


{- conversion -}

toMUType :: Type.Type -> MUType
toMUType = Type.foldType MUTVar MUTConst MUTComp MUTFunc (\t1 t2 -> flatUnion [t1, t2]) MUTRec

toType :: MUType -> Type.Type
toType = foldMUType Type.TVar Type.TConst Type.TComp Type.TFunc (foldr1 Type.TUnion) Type.TRec


{- displaying types -}

instance Show MUType where
  show = foldMUType (\i s -> show i) id (join "@") (join ">") (joinList "(+)") showRec


{- utility functions -}

isDataType :: MUType -> Bool
isDataType t = case t of
                 MUTVar _ D   -> True
                 MUTVar _ T   -> False
                 MUTConst _   -> True
                 MUTFunc _ _  -> False
                 MUTComp d _  -> isDataType d
                 MUTUnion ts  -> and $ map isDataType ts
                 MUTRec t'    -> isDataType t'

{- unfolding -}
unfold :: MUType -> MUType
unfold n@(MUTRec m) = unfold' n m 1

unfold' :: MUType -> MUType -> Index -> MUType
unfold' n m i = case m of
                  MUTVar j s | j > i   ->  MUTVar (j-1) s
                  MUTVar j _ | j == i  ->  incrFreeVars (i-1) n -- (i-1): amount of binders we passed through
                  MUTVar j s | j < i   ->  MUTVar j s
                  MUTConst l   -> MUTConst l
                  MUTFunc a b  -> MUTFunc (unfold' n a i) (unfold' n b i)
                  MUTComp a b  -> MUTComp (unfold' n a i) (unfold' n b i)
                  MUTUnion ts  -> flatUnion $ map (\t' -> unfold' n t' i) ts
                  MUTRec t'    -> MUTRec   $ unfold' n t' (i+1)

incrFreeVars :: Int -> MUType -> MUType
incrFreeVars k m = aux k m 1
  where
    aux k m i = case m of
                  MUTVar j s | j > i  -> MUTVar (j+k) s
                  MUTVar j s | j <= i -> MUTVar j s
                  MUTConst l   -> MUTConst l
                  MUTFunc a b  -> MUTFunc (aux k a i) (aux k b i)
                  MUTComp a b  -> MUTComp (aux k a i) (aux k b i)
                  MUTUnion ts  -> MUTUnion $ map (\t -> aux k t i) ts
                  MUTRec t'    -> MUTRec $ aux k t' (i+1)
