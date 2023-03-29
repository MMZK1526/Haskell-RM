{-# LANGUAGE LambdaCase #-}

module Internal.Response where

import           Data.Char
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as M

newtype Response = Response (Map String Value)
  deriving (Ord, Eq)

data Value = Int { int_ :: Integer } | Bool Bool | String String | Resp Response
           | Values [Value]
  deriving (Ord, Eq)

instance Show Response where
  show (Response resp)
    = concat [ "{"
             , intercalate "," $ map (\(k, v) -> show k ++ ":" ++ show v)
                                     (M.assocs resp)
             , "}" ]

instance Show Value where
  show (Int i)    = show $ show i -- Int is treated as String to avoid precision loss
  show (Bool b)   = toLower <$> show b
  show (Resp r)   = show r
  show (Values v) = show v
  show (String n) = '"' : concatMap show' n ++ "\""
    where
      show' n = case tail $ show n of
        '\\' : '\'' : _ -> "'"
        '"' : _         -> "\\\""
        '\\' : e : _    -> if isDigit e then [n] else init . tail $ show n
        _               -> [n]

size :: Response -> Int
size (Response resp) = length resp

getValue :: Response -> String -> Maybe Value
getValue (Response resp) key = resp M.!? key

getValues :: Response -> String -> Maybe [Value]
getValues resp key = getValue resp key >>= \case
  Values v -> pure v
  _        -> Nothing

getResp :: Response -> String -> Maybe Response
getResp resp key = getValue resp key >>= \case
  Resp r -> pure r
  _      -> Nothing

getBool :: Response -> String -> Maybe Bool
getBool resp key = getValue resp key >>= \case
  Bool r -> pure r
  _      -> Nothing

getString :: Response -> String -> Maybe String
getString resp key = getValue resp key >>= \case
  String r -> pure r
  _        -> Nothing

mkResponse :: [(String, Value)] -> Response
mkResponse = Response . M.fromList

noErr :: Response -> Response
noErr = (mkResponse [("hasError", Bool False)] <>)

mkErrResponse :: [String] -> Response
mkErrResponse errs = mkResponse [ ("hasError", Bool True), ("errors"
                                , Values (String <$> errs)) ]

instance Semigroup Response where
  (Response r1) <> (Response r2) = Response $ r1 `M.union` r2

instance Monoid Response where
  mempty = Response M.empty
