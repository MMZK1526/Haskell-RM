module Internal.LineLike where

import           Internal.Definitions
import           Internal.Line

-- | A type class whose instances can be transformed to a "Line".
class LineLike l where
  toLine   :: l -> Line
  fromLine :: Line -> l

instance LineLike Line where
  toLine   = id
  fromLine = id

instance LineLike Integer where
  toLine   = decodeLine
  fromLine = encodeLine

