module Gadgets.Text where

import           Data.List (foldl')

import qualified Data.Text as T

type Text = T.Text

-- | Apply a function that works on @Text@ to a @String@.
-- 
ftext :: (Text -> Text) -> String -> String
ftext = (. T.pack) . (T.unpack .)

-- | Take a list of pairs of needles and replacements, replace all occurrences
-- of the needles by the replacements.
-- Pre: The needles must be non-empty and disjoint.
-- 
replaceList :: [(Text, Text)] -> Text -> Text
replaceList = flip (foldl' $ flip $ uncurry T.replace)
