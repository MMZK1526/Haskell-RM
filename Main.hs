module Main where

import           Definitions
import           Gadgets.IO
import           Parser
import           RM
import           RMCode
import           System.Environment
import           Text.Read


{-# INLINE help #-}
help :: IO ()
help = putStrLn "Please pass in the path of the source code and the arguments!"

main :: IO ()
main = do
  args <- getArgs
  if      null args
  then    help
  else do
  (file : args) <- return args
  handleDNE ((>> help) . print) $ do
  text <- readFile file
  case rmParser text of
    Left error -> print error -- Error parsing source code
    Right code -> do
    case mapM (readMaybe :: String -> Maybe Integer) args of
      Nothing   -> putStrLn "Error parsing the arguments!"
      Just args -> print $ evalRM code args -- Run the program

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

-- | R0 = R1 + R2.
adder :: RMCode
adder = fromList 
  [ M 1 1 2
  , P 0 0
  , M 2 3 4
  , P 0 2
  , H
  ]

-- | R0 = R1 * R2.
multiplier :: RMCode
multiplier = fromList
  [ M 1 1 6
  , M 2 2 4
  , P 3 3
  , P 0 1
  , M 3 5 0
  , P 2 4
  , H
  ]

-- | R0 = 2^R1 * (2 * R2 + 1).
pusher :: RMCode
pusher = fromList
  [ P 0 1
  , M 2 2 3
  , P 0 0
  , M 0 4 5
  , P 2 3
  , M 1 1 6
  , M 2 7 8
  , P 0 6
  , H
  ]

-- | 2^R0 * (2 * i + 1) = R1.
header :: RMCode
header = fromList
  [ M 1 1 8
  , P 1 2
  , M 1 3 4
  , P 2 2
  , M 2 5 7
  , M 2 6 8
  , P 1 4
  , P 0 2
  , H
  ]

-- | 2^i * (2 * R0 + 1) = R1.
tailer :: RMCode
tailer = fromList
  [ M 1 1 10
  , P 1 2
  , M 1 3 4
  , P 0 2
  , M 0 5 7
  , M 0 6 8
  , P 1 4
  , P 2 2
  , M 1 9 10
  , P 0 8
  , H
  ]

-- | Calculates the number of collatz terms.
collatz :: RMCode
collatz = fromList
  [ M 1 1 22
  , M 1 2 21
  , P 1 3
  , P 1 4
  , M 1 5 7 -- even
  , M 1 6 10 -- odd
  , P 2 4
  , M 2 8 9
  , P 1 7
  , P 0 0
  , P 1 11
  , M 2 12 14
  , P 1 13
  , P 1 11
  , M 1 15 18
  , P 2 16
  , P 2 17
  , P 2 14
  , P 1 19
  , M 2 20 9
  , P 1 19
  , P 0 22
  , H
  ]
  
testP :: RMCode
testP = code
  where
    Right code = rmParser "L0: R1- L2 L1\n\
                          \L1: HALT\n\
                          \L2: R1- L3 L4\n\
                          \L3: R1- L5 L4\n\
                          \L4: HALT\n\
                          \L5: R0+ L0"
