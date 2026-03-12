module Idc.Import
  ( Row(..)
  , ImportError(..)
  , ImportResult(..)
  , parseCatalogCsv
  , parseLine
  , parseCrosswalkCsv
  , renderRow
  , validIdc10Code
  , validIdc11Code
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Char (isDigit, isUpper)
import Data.Text (Text)
import qualified Data.Text as T

data Row = Row
  { rowCode   :: Text
  , rowParent :: Maybe Text
  , rowTitle  :: Text
  , rowExtra  :: Text
  } deriving (Eq, Show)

instance ToJSON Row where
  toJSON (Row c p t e) =
    Aeson.object ["code" Aeson..= c, "parent" Aeson..= p, "title" Aeson..= t, "extra" Aeson..= e]

instance FromJSON Row where
  parseJSON = Aeson.withObject "Row" $ \o ->
    Row <$> o Aeson..: "code" <*> o Aeson..: "parent" <*> o Aeson..: "title" <*> o Aeson..: "extra"

data ImportError = ImportError
  { ieLine :: Int
  , ieRaw  :: Text
  , ieReason :: Text
  } deriving (Eq, Show)

instance ToJSON ImportError where
  toJSON (ImportError ln raw reason) =
    Aeson.object ["line" Aeson..= ln, "raw" Aeson..= raw, "reason" Aeson..= reason]

instance FromJSON ImportError where
  parseJSON = Aeson.withObject "ImportError" $ \o ->
    ImportError <$> o Aeson..: "line" <*> o Aeson..: "raw" <*> o Aeson..: "reason"

data ImportResult = ImportResult
  { irErrors :: [ImportError]
  , irRows   :: [Row]
  } deriving (Eq, Show)

instance ToJSON ImportResult where
  toJSON (ImportResult es rs) =
    Aeson.object ["errors" Aeson..= es, "rows" Aeson..= rs]

instance FromJSON ImportResult where
  parseJSON = Aeson.withObject "ImportResult" $ \o ->
    ImportResult <$> o Aeson..: "errors" <*> o Aeson..: "rows"

parseCatalogCsv :: Text -> ImportResult
parseCatalogCsv txt =
  let ls = drop 1 (T.lines txt)
      numbered = zip [2 :: Int ..] ls
      results = map (uncurry parseCatalogLine) numbered
      errs = [e | Left e <- results]
      rows = [r | Right r <- results]
  in ImportResult errs rows

parseCatalogLine :: Int -> Text -> Either ImportError Row
parseCatalogLine ln raw =
  case parseLine raw of
    Just r  -> Right r
    Nothing -> Left (ImportError ln raw "malformed CSV row")

parseLine :: Text -> Maybe Row
parseLine l =
  case T.splitOn "," (T.strip l) of
    [c, p, t]      -> Just (Row c (opt p) t "")
    [c, p, t, e]   -> Just (Row c (opt p) t e)
    _              -> Nothing

opt :: Text -> Maybe Text
opt p = if T.null p then Nothing else Just p

renderRow :: Row -> Text
renderRow (Row c p t e) =
  T.intercalate "," [c, maybe "" id p, t, e]

parseCrosswalkCsv :: Text -> [(Text, Text, Text)]
parseCrosswalkCsv =
  mapMaybe toTriple . drop 1 . T.lines
  where
    toTriple l = case T.splitOn "," (T.strip l) of
      [a, b, k] -> Just (a, b, k)
      _         -> Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) = case f x of
  Just y  -> y : mapMaybe f xs
  Nothing -> mapMaybe f xs

validChapterRoman :: Text -> Bool
validChapterRoman t =
  not (T.null t) && T.all (`elem` ("IVX" :: String)) t

validNumericChapter :: Text -> Bool
validNumericChapter t =
  T.length t == 2 && T.all isDigit t

validBlockRange :: Text -> Bool
validBlockRange t =
  case T.splitOn "-" t of
    [a, b] -> validStem a && validStem b && T.length a == T.length b
    _      -> False

validStem :: Text -> Bool
validStem t =
  let n = T.length t
  in n >= 3 && n <= 4 && T.all (\c -> isUpper c || isDigit c) t

validIdc10Leaf :: Text -> Bool
validIdc10Leaf t =
  let n = T.length t
  in n >= 3 && n <= 5
     && isUpper (T.head t)
     && T.all isDigit (T.take 2 (T.drop 1 t))
     && (n == 3 || (n == 5 && T.index t 3 == '.' && isDigit (T.index t 4)))

validIdc11Leaf :: Text -> Bool
validIdc11Leaf t =
  case T.splitOn "." t of
    [stem]      -> validStem11 stem
    [stem, ext] -> validStem11 stem && extLen ext && T.all isDigit ext
    _           -> False
  where
    validStem11 s =
      let n = T.length s
      in n == 4
         && T.all (\c -> isUpper c || isDigit c) (T.take 2 s)
         && T.any isUpper (T.take 2 s)
         && T.all isDigit (T.drop 2 s)
    extLen e = not (T.null e) && T.length e <= 2

validIdc10Code :: Text -> Bool
validIdc10Code t
  | validChapterRoman t = True
  | validBlockRange t   = True
  | otherwise           = validIdc10Leaf t

validIdc11Code :: Text -> Bool
validIdc11Code t
  | validNumericChapter t = True
  | validBlockRange t     = True
  | otherwise             = validIdc11Leaf t
