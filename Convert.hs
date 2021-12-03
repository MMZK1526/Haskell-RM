module Convert 
  ( Line(..), LineLike(..), RMCode, decodeLine, decodeList, decodePair
  , decodeRM, encodeLine, encodeList, encodePair, encodeRM, fromList, toList
  ) where

import           Internal.Definitions
import           Internal.Line
import           Internal.LineLike
import           Internal.RMCode
import           Internal.Utilities
