module Typechecker where

import Util
import Fallible

import Type
import Type.Equivalence
import Type.Subtyping
import Type.MUType as MUType
import Term
import Term.Compatibility

import Data.Map as Map
import Data.Set as Set

typeOf :: Ctx -> Term -> Result Type
typeOf ctx t = (muTypeOf ctx t) >>= (return.toType)

muTypeOf :: Ctx -> Term -> Result MUType
muTypeOf ctx (Var l) = case Map.lookup l ctx of
                         Just t  -> return (toMUType t)
                         Nothing -> failVar l ctx

muTypeOf ctx (Const l) = return (MUTConst l)

muTypeOf ctx (App a b) = do
                         ta <- muTypeOf ctx a
                         tb <- muTypeOf ctx b
                         if isDataType ta
                         then
                           return $ MUTComp ta tb
                         else do
                           unfolded <- Typechecker.unfold ta
                           let
                             funcs = MUType.toList unfolded
                             (domains, results) = splitFuncs funcs
                           if all (muSubtype tb) domains
                           then
                             return $ MUType.fromList results
                           else
                             failApp ta tb


muTypeOf ctx (Case branches) = do
                               assignments <- Fallible.map check branches
                               _           <- checkCompatibility assignments
                               resultTypes <- Fallible.map checkBody branches
                               let domainTypes = [ toMUType t | (p,t) <- assignments]
                               return $ MUTFunc (MUType.fromList domainTypes) (MUType.fromList resultTypes)

                               where
                                 check (p,c,t)     = (typeOfPattern c p) >>= (\t -> return (p,t))
                                 checkBody (p,c,t) = do
                                                     _ <- checkDomain c p
                                                     muTypeOf (Map.union c ctx) t

typeOfPattern :: Ctx -> Pattern -> Result Type
typeOfPattern ctx p = case p of
                        PMatch l  -> case Map.lookup l ctx of
                                       Just t  -> return t
                                       Nothing -> failVar l ctx
                        PConst l  -> return $ TConst l
                        PComp a b -> do
                                       ta <- typeOfPattern ctx a
                                       tb <- typeOfPattern ctx b
                                       return $ TComp ta tb

checkCompatibility :: [Assignment] -> Result ()
checkCompatibility [] = return ()
checkCompatibility (a:as) = if and [ compatible a a' | a' <- as]
                            then checkCompatibility as
                            else failure "Incompatible patterns detected"

{- Check that the domain of a binding context equals the free matchables of a pattern -}
checkDomain :: Ctx -> Pattern -> Result ()
checkDomain ctx pattern = if (freeMatchables pattern) == (Set.fromList (Map.keys ctx))
                          then return ()
                          else failure "Invalid binding context for branch body"

freeMatchables :: Pattern -> Set Label
freeMatchables p = Set.fromList $ foldPattern (:[]) (const []) (++) p


unfold :: MUType -> Result MUType
unfold t = case t of
             MUTFunc _ _ -> return t
             MUTUnion ts -> do
                              recs <- Fallible.map Typechecker.unfold ts
                              return (MUTUnion recs)
             MUTRec body -> Typechecker.unfold (MUType.unfold t)
             _           -> failure $ concat ["Type ", show t, " cannot be translated to a function"]

splitFuncs :: [MUType] -> ([MUType], [MUType])
splitFuncs ts = Prelude.foldl splitF ([], []) ts
                where splitF = \(domains, results) (MUTFunc a b) -> (a:domains, b:results)

failVar :: Label -> Ctx -> Result a
failVar l ctx = failure $ concat $ ["Variable ", l, " not found in binding context ", show ctx]

failApp :: MUType -> MUType -> Result a
failApp ta tb = failure $ concat $ ["Invalid application of ", (show tb), " to ", (show ta)]