module Term where

import Util
import Type
import qualified Data.Map.Strict as Map

data Term = Var Label
          | Const Label
          | App Term Term
          | Case [Branch]
          deriving Show

data Pattern = PMatch Label
             | PConst Label
             | PComp Pattern Pattern
             deriving (Eq, Show)

type Branch     = (Pattern, Ctx, Term)
type Assignment = (Pattern,Type)
type Ctx        = Map.Map Label Type

foldPattern :: (Label -> b)
               -> (Label -> b)
               -> (b -> b -> b)
               -> Pattern
               -> b
foldPattern fMatchable fConst fComp p = let rec = foldPattern fMatchable fConst fComp
                                        in case p of
                                            PMatch l -> fMatchable l
                                            PConst l    -> fConst l
                                            PComp p1 p2 -> fComp (rec p1) (rec p2)