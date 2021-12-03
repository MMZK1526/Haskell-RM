module Internal.RMCode where

import qualified Data.Foldable as F
import qualified Gadgets.Array as A
import           Internal.Definitions
import           Internal.LineLike
import           Internal.Utilities

-- | Decodes an "Integer" into a Register Machine "RMCode".
decodeRM :: Integer -> RMCode
decodeRM = fromList . decodeList

-- | Decodes a Register Machine "RMCode" into an "Integer".
encodeRM :: RMCode -> Integer
encodeRM = encodeList . toList

-- | Decodes a list of "Linelike"s (namely "Line" or "Integer") into a Register
-- Machine "RMCode".
fromList :: LineLike l => [l] -> RMCode
fromList = RMCode . A.fromList . fmap toLine

-- | Encodes a Register Machine "RMCode" into a list of "Linelike"s (namely
-- "Line" or "Integer").
toList :: LineLike l => RMCode -> [l]
toList (RMCode rm) = fromLine <$> F.toList rm
