module Gadgets.Array.Mutable where

import           Control.Monad (ap)
import           Data.Array (Array, Ix)
import qualified Data.Array.ST as A
import           Data.Array.ST (MArray)
import qualified Data.Array.Unsafe as A
import qualified Gadgets.Array as A

-- | Making an array from a list, indexed from 0.
fromList :: MArray a e m => [e] -> m (a Int e)
fromList = A.thaw . A.fromList

-- | Adjusts a value in the array with the given function.
-- It will do nothing if the index is out of bound.
adjust :: (Ix i, MArray a e m) => a i e -> (e -> e) -> i -> m ()
adjust arrST f i = do
  x <- arrST ! i
  arrST =: i $ f x

-- | Strict version of "adjust".
adjust' :: (Ix i, MArray a e m) => a i e -> (e -> e) -> i -> m ()
adjust' = (. ap seq) . adjust

-- | Same as @readArray@, but infix.
infixr 4 !
(!) :: (Ix i, MArray a e m) => a i e -> i -> m e
(!) = A.readArray

-- | Safe array access.
infixr 4 !?
(!?) :: (Ix i, MArray a e m) => a i e -> i -> m (Maybe e)
arrST !? i = do
  (inf, sup) <- A.getBounds arrST
  if i < inf || i > sup
    then return Nothing
    else Just <$> A.readArray arrST i

-- | Updates a value in the array.
-- Example: @ arrST =: 3 $ 5 @ sets the third element to five.
-- It will do nothing if the index is out of bound.
infixl 3 =:
(=:) :: (Ix i, MArray a e m) => a i e -> i -> e -> m ()
(=:) arrST i e = do
  (inf, sup) <- A.getBounds arrST
  if i < inf || i > sup
    then return ()
    else A.writeArray arrST i e
