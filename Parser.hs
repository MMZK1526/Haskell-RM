module Parser where

import           Control.Monad (void, liftM2, zipWithM, (>=>))
import           Data.Bifunctor (first)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Definitions
import qualified Gadgets.Array as A
import           Text.Parsec
  ( alphaNum, char, digit, eof, letter, oneOf, parse, string, many, noneOf
  , try, (<|>)
  )
import           Text.Parsec.Error (ParseError)
import           Text.Parsec.String (Parser)
import           Data.Char (isAlpha, isAlphaNum)

type LabelTable = M.Map String Int

-- | A Line of RM code that can contain labels.
data LabelledLine
  = P_ Int (Either Int String)
  | M_ Int (Either Int String) (Either Int String)
  | H_
  deriving Show

data RawRMCode
  = RMCode_ LabelTable [LabelledLine]
  deriving Show

-- | Parses a decimal digit.
parseInt :: Parser Int
parseInt = read <$> do
  f <- oneOf "-0123456789"
  r <- many digit
  if f == '-' && null r
    then fail ""
    else return (f : r)

-- | Parses an alpha-num identifier that starts with a letter.
parseAlphaNum :: Parser String
parseAlphaNum = liftM2 (:) letter $ many (alphaNum <|> char '_')

-- | Ignores zero or many spaces.
eatSpaces :: Parser ()
eatSpaces = void $ many $ char ' '

-- | Parses a "RMCode" from String.
rmParser :: String -> Either String RMCode
rmParser = rmParser' >=> fromRawRMCode

-- | Translates a "RawRMCode" to "RMCode".
fromRawRMCode :: RawRMCode -> Either String RMCode
fromRawRMCode (RMCode_ table ls) = RMCode . A.fromList <$> zipWithM go [0..] ls
  where
    go i l = case l of
      P_ x y   -> P x <$> fetch i y
      M_ x y z -> liftM2 (M x) (fetch i y) (fetch i z)
      H_       -> return H
    fetch _ (Left n) = return n
    fetch i (Right str)
      | str `M.member` table = return $ table M.! str
      | otherwise            = Left $ concat [ "Invalid label "
                                             , str
                                             , " at line "
                                             , show i
                                             , "!"
                                             ]

-- | Parses a "RawRMCode" from String.
rmParser' :: String -> Either String RawRMCode
rmParser' str = first show $ go (lines str) 0
  where
    go [] _ = Right $ RMCode_ M.empty []
    go (l : ls) i
      | all (== ' ') l = go ls $ i + 1
      | otherwise      = do
        RMCode_ table cs <- go ls $ i + 1
        (table, line)    <- parse (parseLine' table i) "RMCode: " l
        return $ RMCode_ table $ line : cs

-- | Parses a single "LabelledLine" of code, taking a label table, the line 
-- number. Updates the table.
parseLine' :: LabelTable -> Int -> Parser (LabelTable, LabelledLine)
parseLine' table i = do
  eatSpaces
  label <- parsePrefix
  table <- case label of
    Nothing -> return table
    Just l  -> if l `M.member` table
    then fail $ "Duplicate label " ++ l ++ " at line " ++ show i ++ "!"
    else return $ M.insert l i table
  eatSpaces
  line <- try (parseM table) <|> try (parseP table) <|> parseH
  eatSpaces
  return (table, line)
  where
    parsePrefix  = do
      try (do
      label <- parseAlphaNum
      eatSpaces
      char ':'
      return $ Just label
      ) <|> return Nothing
    parseLable t = try (Right <$> parseAlphaNum) <|> do
      void (char 'L') <|> return ()
      Left <$> parseInt
    parseP t     = do
      void (char 'R') <|> return ()
      x <- parseInt
      void (char '+') <|> return ()
      char ' '
      eatSpaces
      y <- parseLable t
      return $ P_ x y
    parseM t     = do
      void (char 'R') <|> return ()
      x <- parseInt
      void (char '-') <|> return ()
      char ' '
      eatSpaces
      y <- parseLable t
      char ' '
      eatSpaces
      z <- parseLable t
      return $ M_ x y z
    parseH       = do
      try (string "HALT") <|> string "H"
      eatSpaces
      return H_
