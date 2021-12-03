module Internal.Utilities where

import           Data.List (unfoldr)
import qualified Gadgets.Array as A

decodePair :: (Integral a, Integral b) => b -> (a, a)
decodePair 0 = error "Cannot decode 0!"
decodePair n
  | n < 0     = error "Negative number is invalid!"
  | r == 1    = (0, fromIntegral q)
  | otherwise = let (x, y) = decodePair q in (x + 1, y)
  where
    (q, r) = divMod n 2

decodeList :: Integer -> [Integer]
decodeList = unfoldr go
  where
    go 0 = Nothing
    go n = Just $ decodePair n

encodePair :: (Integral a, Integral b) => a -> a -> b
encodePair x y
  | x < 0 || y < 0 = error "Negative number is invalid!"
  | otherwise      = fromIntegral $ 2 ^ x * (2 * y + 1)

encodeList :: [Integer] -> Integer
encodeList = foldr encodePair 0
