{-# LANGUAGE FlexibleInstances #-}

module Fallible where

import Data.Maybe
import Data.List

type ErrorMsg = String

class (Monad f) => Fallible f where
  failure       :: ErrorMsg -> f v
  isError       :: f v -> Bool
  
  getError      :: f v -> String
  getValue      :: f v -> v

  isSuccess     :: f v -> Bool
  isSuccess     = not.isError
  
  runFallible   :: f v -> v
  runFallible f = if isError f
                   then error $ getError f
                   else getValue f

instance Fallible Maybe where
  failure = const Nothing
  isError  = isNothing
  getError Nothing  = "Unknown error"
  getValue (Just v) = v

instance Fallible (Either ErrorMsg) where
  failure = Left
  isError f = case f of
                Left _  -> True
                _       -> False
  getError (Left msg) = msg
  getValue (Right v) = v


{-
  Left fold of a list with a monadic result type.

  The result of each step is bound to the combine function of the next step.
  This means that the first "error" result generated will be the result ofthe fold.
-}
fold :: Monad m => (b -> a -> m b) -> m b -> [a] -> m b
fold f base = foldr (\a r -> r >>= (\b -> f b a)) base


{-
  Applies a function to each element of a list.
  Returns the list of all unwrapped results, or the first failure.
-}
map :: Monad m => (a -> m b) -> [a] -> m [b]
map f = fold applyAdd (return [])
        where
          applyAdd r a = do
                         fa <- f a
                         return $ fa:r

{-
  Returns the first successful result of a list, or a failure if there is none.
-}
seq :: Fallible m => [m a] -> m a
seq results = case find isSuccess results of
                Nothing  -> failure "No successful result found"
                Just res -> res