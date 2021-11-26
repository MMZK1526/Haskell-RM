module RMCode where

import           Definitions
import qualified Data.Foldable as F
import qualified Gadgets.Array as A
import           Line
import           LineLike
import           Utilities

fromList :: LineLike l => [l] -> RMCode
fromList = RMCode . A.fromList . fmap toLine

decodeRM :: Integer -> RMCode
decodeRM = fromList . decodeList

toList :: LineLike l => RMCode -> [l]
toList (RMCode rm) = fromLine <$> F.toList rm

encodeRM :: RMCode -> Integer
encodeRM = encodeList . toList
