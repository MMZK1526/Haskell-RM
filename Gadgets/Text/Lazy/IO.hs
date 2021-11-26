module Gadgets.Text.Lazy.IO where

import           Control.Monad (liftM2)
import           System.IO as IO 
  (IOMode, Handle, hFlush, hIsEOF, stdin, stdout, withFile)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

type Text = T.Text

-- | Reads all lines from @stdin@, returning a @[Text]@.
--
getLines' :: IO [Text]
getLines' = hGetLines' stdin

-- | Reads all lines from a handle, returning a @[Text]@.
--
hGetLines' :: Handle -> IO [Text]
hGetLines' hdl = do
  b <- hIsEOF hdl
  if b 
    then return []
    else liftM2 (:) (T.hGetLine hdl) (hGetLines' hdl)

-- | Strictly outputs a @Text@ via @stdout@.
--      
putStr' :: Text -> IO ()
putStr' str 
  = T.putStr str >> hFlush stdout

-- | Similar to @withFile@ but takes a @Text@ as file path.
--
withFile' :: Text -> IOMode -> (Handle -> IO a) -> IO a
withFile' = withFile . T.unpack
