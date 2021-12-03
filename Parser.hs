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

-- | A map from negatively-indexed registers to real registers for gadgets.
type RegTable = M.Map Int Int

-- | A Line of RM code that can contain labels.
data LabelledLine
  = P_ Int (Either Int String)
  | M_ Int (Either Int String) (Either Int String)
  | H_
  deriving Show

-- | A Gadget with its signature and labelled body.
data RawGadget
  = Gadget_ Signature LabelTable [LabelledLine]
  deriving Show

-- | Gadget signature: name, input registers, and returning labels.
-- Mapped to negative integers.
data Signature
  = Sig_ String (M.Map String Int) (M.Map String Int)
  deriving Show

data RawRMCode
  = RMCode_ LabelTable [LabelledLine]
  deriving Show

-- | Parses a "RawGadget".
-- parseGadget :: Parser RawGadget
parseGadget = do
  sig <- parseSig
  char '\n'
  -- (t, line) <- parseLine t i
  -- (eof >> return (t, [line])) <|> do
  -- char '\n'
  -- (t, code) <- go t $ i + 1
  -- -- return (t, line : code)
  return sig
  where
    parseLine t i = do
      eatSpaces
      label <- parsePrefix
      table <- case label of
        Nothing -> return t
        Just l  -> if l `M.member` t
        then fail $ "Duplicate label " ++ l ++ " at line " ++ show i ++ "!"
        else return $ M.insert l i t
      eatSpaces
      line <- try parseM <|> try parseP <|> parseH
      eatSpaces
      return (table, line)
    parseP        = do
      void (char 'R') <|> return ()
      x <- parseInt
      void (char '+') <|> return ()
      char ' '
      eatSpaces
      P_ x <$> parseLabel
    parseM        = do
      void (char 'R') <|> return ()
      x <- parseInt
      void (char '-') <|> return ()
      char ' '
      eatSpaces
      y <- parseLabel
      char ' '
      eatSpaces
      M_ x y <$> parseLabel
    parseH        = do
      try (string "HALT") <|> string "H"
      eatSpaces
      return H_
    parseSig      = do
      eatSpaces
      name <- parseAlphaNum
      eatSpaces
      char '('
      args <- parseArgs (-1)
      rets <- parseRets (-1)
      eatSpaces
      char ':'
      eatSpaces
      return $ Sig_ name args rets
    parseArgs i   = do
      eatSpaces
      (char ';' >> return M.empty) <|> do
      a  <- parseAlphaNum
      as <- parseArgs $ i - 1
      if   a `M.member` as
      then fail $ "Duplicate argument " ++ a ++ "!"
      else return $ M.insert a i as
    parseRets i   = do
      eatSpaces
      (char ')' >> return M.empty) <|> do
      r  <- parseAlphaNum
      rs <- parseRets $ i - 1
      if   r `M.member` rs
      then fail $ "Duplicate argument " ++ r ++ "!"
      else return $ M.insert r i rs

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
rmParser = first show <$> parse parseRM "RMCode: " >=> fromRawRMCode

-- | Translates a "RawRMCode" to "RMCode".
fromRawRMCode :: RawRMCode -> Either String RMCode
fromRawRMCode (RMCode_ table ls) = RMCode . A.fromList <$> zipWithM go [0..] ls
  where
    len    = length ls
    go i l = case l of
      P_ x y   -> if x < 0
        then Left $ "Negatively-indexed register at line " ++ show i ++ "!"
        else P x <$> fetch i y
      M_ x y z -> if x < 0
        then Left $ "Negatively-indexed register at line " ++ show i ++ "!"
        else liftM2 (M x) (fetch i y) (fetch i z)
      H_       -> return H
    fetch _ (Left n) = return n
    fetch i (Right str)
      | str `M.member` table = return $ table M.! str
      | otherwise            = return len

-- | Parses a "RawRMCode".
parseRM :: Parser RawRMCode
parseRM = (\x -> let (t, cs) = x in RMCode_ t cs) <$> go M.empty 0
  where
    go t i = (eof >> return (t, [])) <|> do
      (t, line) <- parseLine t i
      (eof >> return (t, [line])) <|> do
      char '\n'
      (t, code) <- go t $ i + 1
      return (t, line : code)

-- | Parse the label of a line.
parsePrefix :: Parser (Maybe String)
parsePrefix = try (do
  label <- parseAlphaNum
  eatSpaces
  char ':'
  return $ Just label
  ) <|> return Nothing

-- | Parse a line number, either an "Int" or a label.
parseLabel :: Parser (Either Int String)
parseLabel = try (Right <$> parseAlphaNum) <|> do
  void (char 'L') <|> return ()
  Left <$> parseInt

-- | Parses a single "LabelledLine" of code, taking a label table, the line 
-- number. Updates the table.
parseLine :: LabelTable -> Int -> Parser (LabelTable, LabelledLine)
parseLine table i = do
  eatSpaces
  label <- parsePrefix
  table <- case label of
    Nothing -> return table
    Just l  -> if l `M.member` table
    then fail $ "Duplicate label " ++ l ++ " at line " ++ show i ++ "!"
    else return $ M.insert l i table
  eatSpaces
  line <- try parseM <|> try parseP <|> parseH
  eatSpaces
  return (table, line)
  where
    parseP      = do
      void (char 'R') <|> return ()
      x <- parseInt
      void (char '+') <|> return ()
      char ' '
      eatSpaces
      P_ x <$> parseLabel
    parseM      = do
      void (char 'R') <|> return ()
      x <- parseInt
      void (char '-') <|> return ()
      char ' '
      eatSpaces
      y <- parseLabel
      char ' '
      eatSpaces
      M_ x y <$> parseLabel
    parseH       = do
      try (string "HALT") <|> string "H"
      eatSpaces
      return H_
