module Substitution where

import Term
import Data.Map as Map
import Data.Set as Set

type Substitution = Map String Pattern

emptySubs :: Substitution
emptySubs = Map.empty

substitute :: String -> Pattern -> Substitution
substitute v q = Map.singleton v q

{- precondition: substitutions are disjoint -}
joinSubs :: Substitution -> Substitution -> Substitution
joinSubs = Map.union

domain :: Substitution -> Set String
domain = keysSet

disjoint :: Substitution -> Substitution -> Bool
disjoint s s' = Set.null $ Set.intersection (domain s) (domain s')

(<.>) :: Substitution -> String -> Pattern
(<.>) s v = case Map.lookup v s of
              Just p -> p
              _      -> PMatch v