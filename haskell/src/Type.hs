module Type where

import Util

{-
  Representation of types in CAP using de Brujin indices.
-}

data Type = TVar Index Sort
          | TConst Label
          | TComp Type Type
          | TFunc Type Type
          | TUnion Type Type
          | TRec Type
          deriving Eq

{- fold types -}

foldType :: (Index -> Sort -> b) -> (Label -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> Type -> b
foldType fVar fConst fComp fFunc fUnion fRec t = let rec = foldType fVar fConst fComp fFunc fUnion fRec
                                                 in case t of
                                                   TVar i s     -> fVar i s
                                                   TConst l     -> fConst l
                                                   TComp t1 t2  -> fComp  (rec t1) (rec t2)
                                                   TFunc t1 t2  -> fFunc  (rec t1) (rec t2)
                                                   TUnion t1 t2 -> fUnion (rec t1) (rec t2)
                                                   TRec t'      -> fRec (rec t')

{- displaying types -}

instance Show Type where
  show = foldType (\i s -> show i) id (join "@") (join ">") (join "(+)") showRec


{-
  Unfolds N = (µ. M):
  
  - decreases free variables of M (j > i) to account for the removal of the binder
  
  - replaces appearances of variables bound by top binder by the term N, increasing
    the free variables in N accordingly to account for the number of binders the variable
    occurs under when substituted
-}
unfold :: Type -> Type
unfold n@(TRec m) = unfold' n m 1

unfold' :: Type -> Type -> Index -> Type
unfold' n m i = case m of
                  TVar j s | j > i   ->  TVar (j-1) s
                  TVar j _ | j == i  ->  incrFreeVars (i-1) n -- (i-1): amount of binders we passed through
                  TVar j s | j < i   ->  TVar j s
                  TConst l   -> TConst l
                  TFunc a b  -> TFunc  (unfold' n a i) (unfold' n b i)
                  TComp a b  -> TComp  (unfold' n a i) (unfold' n b i)
                  TUnion a b -> TUnion (unfold' n a i) (unfold' n b i)
                  TRec t'    -> TRec   $ unfold' n t' (i+1)

{-
  When replacing M by a variable nested under k binders, we increase the index
  of all free variables by k to preserve their meaning.
-}
incrFreeVars :: Int -> Type -> Type
incrFreeVars k m = aux k m 1
  where
    aux k m i = case m of
                  TVar j s | j > i  -> TVar (j+k) s
                  TVar j s | j <= i -> TVar j s
                  TConst l   -> TConst l
                  TFunc a b  -> TFunc  (aux k a i) (aux k b i)
                  TComp a b  -> TComp  (aux k a i) (aux k b i)
                  TUnion a b -> TUnion (aux k a i) (aux k b i)
                  TRec t'    -> TRec $ aux k t' (i+1)
