module Main where

import           Control.Monad
import           Data.Char
import           Data.List
import           Gadgets.IO
import           Internal.Definitions
import           Internal.Line
import           Internal.LineLike
import           Internal.Parser
import           Internal.RM
import           Internal.RMCode
import           Internal.Utilities
import           System.Console.GetOpt
import           System.Environment
import           Text.Read

data CLIOption = I0 | Detail Int | Error String
  deriving (Eq, Show)

data CLIConfig = CLIOptions { isI0 :: Bool
                            , detailSteps :: Maybe Int
                            , errors :: [String] }
  deriving (Eq, Show)

mkConfig :: [CLIOption] -> CLIConfig
mkConfig = foldl go
         $ CLIOptions { isI0 = False, detailSteps = Nothing, errors = [] }
  where
    go opts I0         = opts { isI0 = True }
    go opts (Detail n) = case detailSteps opts of
      Nothing -> opts { detailSteps = Just n }
      _       -> opts
    go opts (Error e)  = opts { errors = e : errors opts }

{-# INLINE help #-}
help :: IO ()
help = do
  putStrLn "\nUsage: mmzkrm {<options>} <src_file.rm> {<arguments>}"
  putStrLn "RM File Format: TODO"
  putStrLn "Arguments:\n  A list of non-negative positive integers assigned to \
           \the registers, starting from R1; R0 is set to 0."
  putStrLn (usageInfo "Options:" optionTable)

optionTable :: [OptDescr CLIOption]
optionTable = [ Option "i" [] (NoArg I0) "Starts the arguments from R0."
              , Option "s" ["step"] (OptArg intDef20 "20") "Show the \
                \configuration after each step of evaluation. \"--step=x\" \
                \shows x steps at a time. Enter \"quit\" to jump to the \
                \result." ]
  where
    intDef20 Nothing    = Detail 20
    intDef20 (Just str) = case readMaybe str of
      Nothing -> errMsg
      Just n  -> if n > 0 then Detail n else errMsg
      where
        errMsg = Error "The argument for steps must be a positive integer!"

main :: IO ()
main = do
  (opts, rawArgs, errs) <- getOpt Permute optionTable <$> getArgs
  let config = mkConfig opts
  if   null rawArgs || not (null $ errs ++ errors config)
  then forM_ errs putStr >> forM_ (nub (errors config)) putStrLn >> help
  else do
    file : args <- return rawArgs
    handleDNE ((>> help) . print) $ do
    text <- readFile file
    case rmParser text of
      Left error -> putStrLn error >> help -- Error parsing source code
      Right code -> case mapM (readMaybe :: String -> Maybe Integer) args of
        Nothing   -> putStrLn "Error parsing the arguments!" >> help
        Just args -> if any (< 0) args -- Run the program
          then putStrLn "The arguments must be non-negative!" >> help
          else do
            let args'     = if isI0 config then args else 0 : args
            let showRes r = do
                  putStrLn $ "Execution finished after "
                          ++ show (resSteps r)
                          ++ if resSteps r == 1 then "step." else " steps."
                  putStrLn "Register values: "
                  forM_ (zip [0..] $ resRegs r) $ \(i, r) ->
                    putStrLn $ "  R" ++ show i ++ ": " ++ show r
            result <- case detailSteps config of
              Nothing   -> pure $ runRM code args'
              Just step -> do
                rm <- initRMIO code args'
                let go i rm = do
                      (rm, mResult) <- runRMIO rm (i, step)
                      case mResult of
                        Just r  -> pure r
                        Nothing -> do
                          l <- getLine
                          case toLower <$> l of
                            "q"    -> pure $ runRM code args'
                            "quit" -> pure $ runRM code args'
                            _      -> go (i + step) rm
                go 1 rm
            showRes result


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
