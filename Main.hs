module Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Array
import           Data.Char
import           Data.Functor
import           Data.Maybe
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
import           Text.Read (readMaybe)

data CLIOption = I0 | Detail Int | Error String | Job Job | FORCED
  deriving (Eq, Show)
data Job = Decode | Encode | Simulate
  deriving (Eq, Show)

data CLIConfig = CLIOptions { isI0        :: Bool
                            , detailSteps :: Maybe Int
                            , job         :: Job
                            , errors      :: [String]
                            , forced      :: Bool }
  deriving (Eq, Show)

mkConfig :: [CLIOption] -> CLIConfig
mkConfig = foldl go
         $ CLIOptions { isI0 = False, detailSteps = Nothing, errors = []
                      , job = Simulate, forced = False }
  where
    go opts I0         = opts { isI0 = True }
    go opts FORCED     = opts { forced = True }
    go opts (Detail n) = case detailSteps opts of
      Nothing -> opts { detailSteps = Just n }
      _       -> opts
    go opts (Error e)  = opts { errors = e : errors opts }
    go opts (Job j)    = case job opts of
      Decode   -> opts
      Encode   -> opts
      Simulate -> opts { job = j }

{-# INLINE help #-}
help :: IO ()
help = do
  putStrLn "See https://github.com/sorrowfulT-Rex/Haskell-RM for a full \
           \documentation.\n"
  putStrLn "Usage:"
  putStrLn "  Simulation\tmmzkrm {<options>} <src_file.rm> {<argument>}"
  putStrLn "  Decoding\tmmzkrm -d <gödel_number>\n"
  putStrLn "  Encoding\tmmzkrm -e (<src_file.rm> | {<argument>})\n"
  putStrLn "Arguments:"
  putStrLn "  <src_file.rm>\tThe path to source file containing a register \
           \machine."
  putStrLn "  {<argument>}\tA list of non-negative integers assigned to the \
           \registers, starting from R1; R0 is set to 0. Unspecified arguments \
           \default to 0.\n"
  putStrLn (usageInfo "Options:" optionTable)

optionTable :: [OptDescr CLIOption]
optionTable
  = [ Option "i" ["initial"] (NoArg I0) "Starts the arguments from R0."
    , Option "s" ["step"] (OptArg intDef20 "20") "Show the configuration after \
      \each step of evaluation. \"--step=x\" shows x steps at a time. Enter \
      \\"quit\" to jump to the result."
    , Option "d" ["decode"] (NoArg (Job Decode)) "Decode the following Gödel \
      \number."
    , Option "e" ["encode"] (NoArg (Job Encode)) "Encode the input, which \
      \could be a list of numbers separated by spaces, a pair of numbers, or \
      \the the path to a source file. By default, if the resultant number is \
      \too large, it will not be shown."
    , Option "f" ["force"] (NoArg FORCED) "Used with the encode option. Show \
      \the result regardless of its size. Note that this may cause the program \
      \to stall indefinitely if the number is too large." ]
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
  if not (null $ errs ++ errors config)
    then forM_ errs putStr >> forM_ (nub (errors config)) putStrLn >> help
    else case job config of
      Simulate -> execute config rawArgs
      Decode   -> decode config rawArgs
      Encode   -> encode config rawArgs

decode :: CLIConfig -> [String] -> IO ()
decode config []   = putStrLn "Please enter an argument!\n" >> help
decode config args = do
  case readMaybe (head args) of
    Nothing -> putStrLn "The Gödel number must be non-negative!\n" >> help
    Just n  -> do
      putStrLn $ if n == 0
        then "Cannot decode 0 into pairs."
        else "Decode to pair: " ++ show (decodePair n)
      putStrLn $ "Decode to list: " ++ show (decodeList n)
      putStrLn $ "Decode to line: " ++ show (decodeLine n)
      putStrLn "Decode to Register Machine: "
      print $ decodeRM n

encode :: CLIConfig -> [String] -> IO ()
encode config []   = putStrLn "Please enter an argument!\n" >> help
encode config args = do
  result <- runExceptT $ msum [asCode (head args), asList args]
  case result of
    Left _  -> putStrLn "Cannot parse arguments as file path or number list!\n"
            >> help
    Right _ -> pure ()
  where
    isForced    = forced config
    asCode arg  = do
      code@(RMCode arr) <- ExceptT $ openRM arg
      let lineCodes = flip map (elems arr) $ \line -> case line of 
            P n i   -> guard (n <= 6251 || isForced) $> encodeLine line
            M n i j -> guard ((n <= 6251 && i <= 6251) || isForced)
                    $> encodeLine line
            H       -> Just $ encodeLine line
      lift . putStrLn $ "Encode each line: " ++ "["
        ++ intercalate ", " (maybe "<large number>" show <$> lineCodes) ++ "]"
      lift $ if sum (succ <$> catMaybes lineCodes) > 6251 && not isForced
        then putStrLn "The Gödel number of this Register Machine is too large."
        else putStrLn $ "Gödel number: " ++ show (encodeRM code)
    asList args = do
      list <- except $ maybe (Left "") Right (sequence $ readMaybe <$> args)
      lift $ case list of
        [x, y] -> if x > 6251 && not isForced
          then putStrLn "The encoding of this pair is too large."
          else putStrLn $ "Encode from pair: " ++ show (encodePair x y)
        _      -> pure ()
      lift $ if sum (succ <$> list) > 6251 && not isForced
        then putStrLn "The encoding of this list is too large."
        else putStrLn $ "Encode from list: " ++ show (encodeList list)

openRM :: String -> IO (Either String RMCode)
openRM path = handleDNE (pure . Left . show) $ rmParser <$> readFile path

execute :: CLIConfig -> [String] -> IO ()
execute config []      = putStrLn "Please enter an argument!\n" >> help
execute config rawArgs = do
  file : args <- return rawArgs
  ecode       <- openRM file
  case ecode of
    Left error -> print error >> help -- Error parsing source code
    Right code -> case mapM (readMaybe :: String -> Maybe Integer) args of
      Nothing   -> putStrLn "Error parsing the arguments!\n" >> help
      Just args -> if any (< 0) args -- Run the program
        then putStrLn "The arguments must be non-negative!\n" >> help
        else do
          let args'          = if isI0 config then args else 0 : args
          let showRes RMLoop = putStrLn "The machine never terminates due to \
                                        \an infinite loop!"
              showRes r      = do
                putStrLn $ "Execution finished after "
                        ++ show (resSteps r)
                        ++ if resSteps r == 1 then " step." else " steps."
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
