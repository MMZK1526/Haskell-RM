module Internal.RMCode where

import qualified Data.Foldable as F
import qualified Gadgets.Array as A
import           Internal.Definitions
import           Internal.LineLike
import           Internal.Utilities

fromList :: LineLike l => [l] -> RMCode
fromList = RMCode . A.fromList . fmap toLine

decodeRM :: Integer -> RMCode
decodeRM = fromList . decodeList

toList :: LineLike l => RMCode -> [l]
toList (RMCode rm) = fromLine <$> F.toList rm

encodeRM :: RMCode -> Integer
encodeRM = encodeList . toList
