module Internal.Line where

import           Internal.Definitions
import           Internal.Utilities

-- | Decodes an "Integer" into a "Line".
decodeLine :: Integer -> Line
decodeLine 0 = H
decodeLine n
  = let (x, y) = decodePair n
        x'     = x `div` 2
    in  if   even x
        then P x' y
        else let (p, q) = decodePair (y + 1) in M x' p q

-- | Encodes a "Line" into an "Integer".
encodeLine :: Line -> Integer
encodeLine H         = 0
encodeLine (P x y)   = encodePair (2 * x) y
encodeLine (M x y z) = encodePair (2 * x + 1) $
                                  encodePair y z - 1
