module Parser where

import           Control.Monad (void, liftM2, zipWithM)
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

-- | Parses an "RMCode".
rmParser :: String -> Either String RMCode
rmParser str = do
  let ls = lines str
  table <- genTable 0 ls
  fmap (RMCode . A.fromList) $
       first show $
             mapM (parse (parseLine table) "RMCode: ") ls
  where
    getLabel            = foldr go Nothing
    go ':' _            = Just ""
    go ' ' cs           = cs
    go c   cs           = (c :) <$> cs
    isValid (l : ls)    = isAlpha l && all isAlphaNum ls
    isValid _           = False
    genTable _ []       = Right M.empty
    genTable i (l : ls) = do
      table <- genTable (i + 1) ls
      case getLabel l of
        Nothing -> return table
        Just l' -> do
          if not $ isValid l'
          then    Left $
            "Invalid label \"" ++ l' ++ "\" at line " ++ show i ++ "!"
          else if M.member l' table
          then    Left $
            "Duplicate label " ++ l' ++ " at line " ++ show i ++ "!"
          else    return $ M.insert l' i table

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
parseAlphaNum = liftM2 (:) letter $ many alphaNum

-- | Ignores zero or many spaces.
eatSpaces :: Parser ()
eatSpaces = void $ many $ char ' '

-- | Parses a single "Line" of code given the label table.
parseLine :: M.Map String Int -> Parser Line
parseLine table = (<* eof) $ (prefix >>) $ parseHalt <|> do
  void (char 'R') <|> return ()
  i <- parseInt
  eatSpaces
  j <- parseLabel
  eatSpaces
  try (do
  k <- parseLabel
  eatSpaces
  return $ M i j k) <|> return (P i j)
  where
    parseLabel = try (do
      label <- try parseAlphaNum
      case table M.!? label of
        Just l  -> return l
        Nothing -> fail $ "Invalid label " ++ label
      ) <|> ((void (char 'L') <|> return ()) >> parseInt)
    parseHalt  = do
      try (string "HALT") <|> string "H"
      eatSpaces
      return H
    -- Ignore labels (if any).
    prefix     = do
      try (do
      many $ noneOf ":"
      void $ char ':') <|> return ()
      eatSpaces
