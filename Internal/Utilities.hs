module Internal.Utilities where

import           Data.List (unfoldr)
import qualified Gadgets.Array as A

-- | Decodes an "Integral" into a pair of "Integrals".
decodePair :: (Integral a, Integral b) => b -> (a, a)
decodePair 0 = error "Cannot decode 0!"
decodePair n
  | n < 0     = error "Negative number is invalid!"
  | r == 1    = (0, fromIntegral q)
  | otherwise = let (x, y) = decodePair q in (x + 1, y)
  where
    (q, r) = divMod n 2

-- | Decodes an "Integer" into a list of "Integer"s.
decodeList :: Integer -> [Integer]
decodeList = unfoldr go
  where
    go 0 = Nothing
    go n = Just $ decodePair n

-- | Decodes a pair of "Integrals" into an "Integral".
encodePair :: (Integral a, Integral b) => a -> a -> b
encodePair x y
  | x < 0 || y < 0 = error "Negative number is invalid!"
  | otherwise      = fromIntegral $ 2 ^ x * (2 * y + 1)

-- | Encodes a list of "Integer"s into an "Integer".
encodeList :: [Integer] -> Integer
encodeList = foldr encodePair 0
