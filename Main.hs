module Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Array
import           Data.Char
import           Data.Functor
import           Data.Maybe
import           Data.List
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           Gadgets.IO
import           Internal.Definitions
import           Internal.Line
import           Internal.LineLike
import           Internal.Parser
import           Internal.Response
import           Internal.RM
import           Internal.RMCode
import           Internal.Utilities
import           System.Console.GetOpt
import           System.Environment
import           Text.Read (readMaybe)

data CLIOption = I0 | Detail Int | Error String | Job Job | Forced | JSON
  deriving (Eq, Show)
data Job = Decode | Encode | Simulate
  deriving (Eq, Show)

data CLIConfig = CLIOptions { isI0        :: Bool
                            , detailSteps :: Maybe Int
                            , job         :: Job
                            , errors      :: [String]
                            , forced      :: Bool
                            , useJSON     :: Bool }
  deriving (Eq, Show)

mkConfig :: [CLIOption] -> CLIConfig
mkConfig = foldl go
         $ CLIOptions { isI0 = False, detailSteps = Nothing, errors = []
                      , job = Simulate, forced = False, useJSON = False }
  where
    go opts JSON       = opts { useJSON = True }
    go opts I0         = opts { isI0 = True }
    go opts Forced     = opts { forced = True }
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

jsonOption :: OptDescr CLIOption
jsonOption = Option "j" ["json"] (NoArg JSON) ""

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
    , Option "f" ["force"] (NoArg Forced) "Used with the encode option. Show \
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
  setLocaleEncoding utf8
  (opts, rawArgs, errs) <- getOpt Permute (jsonOption : optionTable) <$> getArgs
  let config = mkConfig opts
  if not (null $ errs ++ errors config)
    then if useJSON config
      then print (mkErrResponse $ errs ++ errors config)
      else forM_ errs putStr >> forM_ (nub (errors config)) putStrLn >> help
    else case job config of
      Simulate -> execute config rawArgs
      Decode   -> decode config rawArgs
      Encode   -> encode config rawArgs

decode :: CLIConfig -> [String] -> IO ()
decode config []   = if useJSON config
  then print (mkErrResponse ["Please enter an argument!"])
  else putStrLn "Please enter an argument!\n" >> help
decode config args = case readMaybe (head args) of
  Nothing -> if useJSON config
    then print (mkErrResponse ["The Gödel number must be non-negative!"])
    else putStrLn "The Gödel number must be non-negative!\n" >> help
  Just n  -> do
    let pairResp = mkResponse
          [ ("decodeToPair", let (x, y) = decodePair n
                             in  Values [Int x, Int y]) | n /= 0 ]
    let listResp = mkResponse
          [("decodeToList", Values $ Int <$> decodeList n)]
    let rmResp   = mkResponse [("decodeToRM", String . show $ decodeRM n)]
    if useJSON config
      then print (noErr $ pairResp <> listResp <> rmResp)
      else do
        putStrLn $ case getValues pairResp "decodeToPair" of
          Just [Int x, Int y] -> "Decode to pair: " ++ show (x, y)
          _                   -> "Cannot decode 0 into pairs."
        putStr "Decode to list: "
        print $ int_ <$> fromJust (getValues listResp "decodeToList")
        putStrLn "Decode to Register Machine: "
        putStrLn . fromJust $ getString rmResp "decodeToRM"

encode :: CLIConfig -> [String] -> IO ()
encode config args = do
  result <- runExceptT $ msum [asCode (head $ args ++ [""]), asList args]
  case result of
    Left errMsg -> if useJSON config
      then print $ mkErrResponse [errMsg]
      else putStrLn errMsg >> putStrLn "" >> help
    Right r     -> if useJSON config
      then print $ noErr r
      else void . runMaybeT $ msum [fromCode r, fromList r]
  where
    mkMaybeResp Nothing
      = mkResponse [("isTooBig", Bool True)]
    mkMaybeResp (Just i)
      = mkResponse [("isTooBig", Bool False), ("num", Int i)]
    isForced    = forced config
    asCode arg  = do
      code@(RMCode arr) <- ExceptT $ openRM arg
      let lineCodes = flip map (elems arr) $ \line -> case line of
            P n i   -> guard (n <= 114514 || isForced) $> encodeLine line
            M n i j -> guard ((n <= 114514 && i <= 114514) || isForced)
                    $> encodeLine line
            H       -> Just $ encodeLine line
      lift $ do
        let lineResp  = mkResponse
              [("encodeToLine", Values $ Resp . mkMaybeResp <$> lineCodes)]
        let godelResp = mkResponse
              $ if sum (succ <$> catMaybes lineCodes) > 114514 && not isForced
              then [("isTooBig", Bool True)]
              else [("isTooBig", Bool False), ("num", Int $ encodeRM code)]
        return $ lineResp <> mkResponse [("encodeFromRM", Resp godelResp)]
    asList args = do
      list <- except $ maybe (Left "") Right (mapM readMaybe args)
      let pairResp = mkResponse $ case list of
            [x, y] -> if x > 114514 && not isForced
              then [("isTooBig", Bool True)]
              else [("isTooBig", Bool False), ("num", Int $ encodePair x y)]
            _      -> []
      let listResp = mkResponse $ if sum (succ <$> list) > 114514 && not isForced
              then [("isTooBig", Bool True)]
              else [("isTooBig", Bool False), ("num", Int $ encodeList list)]
      return . mkResponse
             $ ("encodeFromList", Resp listResp)
             : [("encodeFromPair", Resp pairResp) | size pairResp > 0]
    fromCode r  = do
      lineEncode <- MaybeT . return $ getValues r "encodeToLine"
      lift $ putStrLn "Encode each line: "
      lift . forM_ lineEncode $ \line -> do
        putStr "  "
        let Resp r = line
        putStrLn $ if fromJust $ getBool r "isTooBig"
          then "<large number>"
          else read . show . fromJust $ getValue r "num"
      rmEncode   <- MaybeT . return $ getResp r "encodeFromRM"
      lift . putStrLn $ if fromJust $ getBool rmEncode "isTooBig"
        then "The Gödel number of this Register Machine is too large."
        else "Gödel number: "
          ++ read (show (fromJust $ getValue rmEncode "num"))
    fromList r  = do
      listEncode <- MaybeT . return $ getResp r "encodeFromList"
      lift . putStrLn $ if fromJust $ getBool listEncode "isTooBig"
        then "The encoding of this list is too large."
        else "Encode from list: "
          ++ read (show (fromJust $ getValue listEncode "num"))
      case getResp r "encodeFromPair" of
        Nothing         -> pure ()
        Just pairEncode -> lift . putStrLn
                         $ if fromJust $ getBool pairEncode "isTooBig"
          then "The encoding of this pair is too large."
          else "Encode from pair: "
            ++ read (show (fromJust $ getValue pairEncode "num"))

openRM :: String -> IO (Either String RMCode)
openRM path = handleIO (pure . Left . show) $ rmParser <$> readFile path

execute :: CLIConfig -> [String] -> IO ()
execute config []      = if useJSON config
  then print (mkErrResponse ["Please enter an argument!"])
  else putStrLn "Please enter an argument!\n" >> help
execute config rawArgs = do
  file : args <- return rawArgs
  ecode       <- openRM file
  case ecode of
    Left error -> if useJSON config
      then print $ mkErrResponse [error]
      else putStrLn error >> help
    Right code -> case mapM (readMaybe :: String -> Maybe Integer) args of
      Nothing  -> if useJSON config
        then print $ mkErrResponse ["Error parsing the arguments!"]
        else putStrLn "Error parsing the arguments!\n" >> help
      Just args -> if any (< 0) args -- Run the program
        then if useJSON config
          then print $ mkErrResponse ["The arguments must be non-negative!"]
          else putStrLn "The arguments must be non-negative!\n" >> help
        else do
          let args'          = if isI0 config then args else 0 : args
          let showRes RMLoop = if useJSON config
                then print $ mkErrResponse ["The machine never terminates due to \
                                            \an infinite loop!"]
                else putStrLn "The machine never terminates due to \
                              \an infinite loop!"
              showRes r      = if useJSON config
                then do
                  let stepResp = mkResponse [("steps", Int $ resSteps r)]
                  let regResp  = mkResponse [("registerValues", Values (Int <$> resRegs r))]
                  print $ noErr $ stepResp <> regResp
                else do
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
