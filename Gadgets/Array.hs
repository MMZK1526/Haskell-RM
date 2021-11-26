module Gadgets.Array where

import           Control.Monad (ap)
import           Data.Array (Array, Ix, bounds, listArray, (!), (//))

-- | Making an array from a list, indexed from 0.
fromList :: [a] -> Array Int a
fromList xs = listArray (0, length xs - 1) xs

-- | Adjusts a value in the array with the given function.
-- It will do nothing if the index is out of bound.
adjust :: Ix i => Array i a -> (a -> a) -> i -> Array i a
adjust arr f i = arr // [(i, f $ arr ! i)]

-- | Strict version of "adjust".
adjust' :: Ix i => Array i a -> (a -> a) -> i -> Array i a
adjust' = (. ap seq) . adjust

-- | Safe array access.
infixr 4 !?
(!?) :: Ix i => Array i a -> i -> Maybe a
arr !? i 
  | i < inf || i > sup = Nothing
  | otherwise          = Just $ arr ! i
  where
     (inf, sup) = bounds arr
