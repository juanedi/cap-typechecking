module Util where

import Data.List

data Sort  = D | T deriving Eq
type Index = Int
type Label = String

showRec :: String -> String
showRec r  = "(µ. " ++ r ++ ")"

join :: String -> String -> String -> String
join op l r = "(" ++ l ++ " " ++ op ++ " " ++ r ++ ")"

joinList :: String -> [String] -> String
joinList op = foldr1 (\s rec->  "(" ++ s ++ " " ++ op ++ " " ++ rec ++ ")")
