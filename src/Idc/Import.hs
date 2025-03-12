module Idc.Import
  ( Row(..)
  , parseCatalogCsv
  , parseLine
  , parseCrosswalkCsv
  , renderRow
  , validIdc10Code
  , validIdc11Code
  ) where

import Data.Char (isDigit, isUpper)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

data Row = Row
  { rowCode   :: Text
  , rowParent :: Maybe Text
  , rowTitle  :: Text
  , rowExtra  :: Text
  } deriving (Eq, Show)

parseCatalogCsv :: Text -> [Row]
parseCatalogCsv =
  mapMaybe parseLine . drop 1 . T.lines

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
