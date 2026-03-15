module Idc.Search
  ( Hit(..)
  , scoreHit
  , rankHits
  , normalizeText
  ) where

import Data.Char (ord)
import Data.Foldable (foldr')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

data Hit = Hit
  { hitCode  :: Text
  , hitTitle :: Text
  } deriving (Eq, Show)

scoreHit :: Text -> Hit -> Double
scoreHit q h =
  exact + codeBonus + wordBonus - lengthPenalty
  where
    ql             = fromIntegral (T.length q) :: Double
    tl             = fromIntegral (T.length (hitTitle h)) :: Double
    exact          = if T.toLower (hitTitle h) == T.toLower q then 100 else 0
    codeBonus      = if q `T.isPrefixOf` hitCode h then 40 - ql * 2 else 0
    titleWords     = map T.toLower (T.words (hitTitle h))
    wordBonus
      | any (T.toLower q `T.isPrefixOf`) titleWords = 30
      | T.toLower q `T.isInfixOf` T.toLower (hitTitle h) = 10
      | otherwise = 0
    lengthPenalty  = tl / 200.0

rankHits :: Text -> [Hit] -> [Hit]
rankHits q =
  map snd . sortDesc (scoreHit q . snd) . zip [0 :: Int ..]

sortDesc :: Ord b => (a -> b) -> [a] -> [a]
sortDesc f = foldr' insert []
  where
    insert x [] = [x]
    insert x (y : ys)
      | f y < f x = x : y : ys
      | otherwise = y : insert x ys

normalizeText :: Text -> Text
normalizeText = T.concatMap foldChar
  where
    foldChar c
      | Just r <- Map.lookup (ord c) table = r
      | ord c >= 0xC0 && ord c <= 0xC5 = T.singleton 'A'
      | ord c >= 0xC8 && ord c <= 0xCB = T.singleton 'E'
      | ord c >= 0xCC && ord c <= 0xCF = T.singleton 'I'
      | ord c >= 0xD2 && ord c <= 0xD6 = T.singleton 'O'
      | ord c >= 0xD9 && ord c <= 0xDC = T.singleton 'U'
      | ord c == 0xD1 = T.singleton 'N'
      | ord c == 0xDD = T.singleton 'Y'
      | ord c >= 0xE0 && ord c <= 0xE5 = T.singleton 'a'
      | ord c >= 0xE8 && ord c <= 0xEB = T.singleton 'e'
      | ord c >= 0xEC && ord c <= 0xEF = T.singleton 'i'
      | ord c >= 0xF2 && ord c <= 0xF6 = T.singleton 'o'
      | ord c >= 0xF9 && ord c <= 0xFC = T.singleton 'u'
      | ord c == 0xF1 = T.singleton 'n'
      | ord c == 0xFD || ord c == 0xFF = T.singleton 'y'
      | otherwise = T.singleton c
    table :: Map.Map Int Text
    table = Map.fromList
      [ (0x0100, "A"), (0x0101, "a"), (0x0102, "A"), (0x0103, "a")
      , (0x0104, "A"), (0x0105, "a"), (0x0106, "C"), (0x0107, "c")
      , (0x0108, "C"), (0x0109, "c"), (0x010A, "C"), (0x010B, "c")
      , (0x010C, "C"), (0x010D, "c"), (0x010E, "D"), (0x010F, "d")
      , (0x0110, "D"), (0x0111, "d"), (0x0112, "E"), (0x0113, "e")
      , (0x0114, "E"), (0x0115, "e"), (0x0116, "E"), (0x0117, "e")
      , (0x0118, "E"), (0x0119, "e"), (0x011A, "E"), (0x011B, "e")
      , (0x011C, "G"), (0x011D, "g"), (0x011E, "G"), (0x011F, "g")
      , (0x0120, "G"), (0x0121, "g"), (0x0122, "G"), (0x0123, "g")
      , (0x0124, "H"), (0x0125, "h"), (0x0128, "I"), (0x0129, "i")
      , (0x012A, "I"), (0x012B, "i"), (0x012C, "I"), (0x012D, "i")
      , (0x012E, "I"), (0x012F, "i"), (0x0130, "I"), (0x0131, "i")
      , (0x0132, "IJ"), (0x0133, "ij"), (0x0134, "J"), (0x0135, "j")
      , (0x0136, "K"), (0x0137, "k"), (0x0139, "L"), (0x013A, "l")
      , (0x013B, "L"), (0x013C, "l"), (0x013D, "L"), (0x013E, "l")
      , (0x013F, "L"), (0x0140, "l"), (0x0141, "L"), (0x0142, "l")
      , (0x0143, "N"), (0x0144, "n"), (0x0145, "N"), (0x0146, "n")
      , (0x0147, "N"), (0x0148, "n"), (0x0149, "n")
      , (0x014A, "N"), (0x014B, "n"), (0x014C, "O"), (0x014D, "o")
      , (0x014E, "O"), (0x014F, "o"), (0x0150, "O"), (0x0151, "o")
      , (0x0152, "OE"), (0x0153, "oe"), (0x0154, "R"), (0x0155, "r")
      , (0x0156, "R"), (0x0157, "r"), (0x0158, "R"), (0x0159, "r")
      , (0x015A, "S"), (0x015B, "s"), (0x015C, "S"), (0x015D, "s")
      , (0x015E, "S"), (0x015F, "s"), (0x0160, "S"), (0x0161, "s")
      , (0x0162, "T"), (0x0163, "t"), (0x0164, "T"), (0x0165, "t")
      , (0x0166, "T"), (0x0167, "t"), (0x0168, "U"), (0x0169, "u")
      , (0x016A, "U"), (0x016B, "u"), (0x016C, "U"), (0x016D, "u")
      , (0x016E, "U"), (0x016F, "u"), (0x0170, "U"), (0x0171, "u")
      , (0x0172, "U"), (0x0173, "u"), (0x0174, "W"), (0x0175, "w")
      , (0x0176, "Y"), (0x0177, "y"), (0x0178, "Y"), (0x0179, "Z")
      , (0x017A, "z"), (0x017B, "Z"), (0x017C, "z"), (0x017D, "Z")
      , (0x017E, "z"), (0x017F, "s")
      , (0x1E00, "A"), (0x1E01, "a"), (0x1E02, "B"), (0x1E03, "b")
      , (0x1E04, "B"), (0x1E05, "b"), (0x1E06, "B"), (0x1E07, "b")
      , (0x1E08, "C"), (0x1E09, "c"), (0x1E0A, "D"), (0x1E0B, "d")
      , (0x1E0C, "D"), (0x1E0D, "d"), (0x1E0E, "D"), (0x1E0F, "d")
      , (0x1E10, "D"), (0x1E11, "d"), (0x1E12, "D"), (0x1E13, "d")
      , (0x1E14, "E"), (0x1E15, "e"), (0x1E16, "E"), (0x1E17, "e")
      , (0x1E18, "E"), (0x1E19, "e"), (0x1E1A, "E"), (0x1E1B, "e")
      , (0x1E1C, "E"), (0x1E1D, "e"), (0x1E1E, "F"), (0x1E1F, "f")
      , (0x1E20, "G"), (0x1E21, "g"), (0x1E22, "H"), (0x1E23, "h")
      , (0x1E24, "H"), (0x1E25, "h"), (0x1E26, "H"), (0x1E27, "h")
      , (0x1E28, "H"), (0x1E29, "h"), (0x1E2A, "H"), (0x1E2B, "h")
      , (0x1E2C, "I"), (0x1E2D, "i"), (0x1E2E, "I"), (0x1E2F, "i")
      , (0x1E30, "K"), (0x1E31, "k"), (0x1E32, "K"), (0x1E33, "k")
      , (0x1E34, "K"), (0x1E35, "k"), (0x1E36, "L"), (0x1E37, "l")
      , (0x1E38, "L"), (0x1E39, "l"), (0x1E3A, "L"), (0x1E3B, "l")
      , (0x1E3C, "L"), (0x1E3D, "l"), (0x1E3E, "M"), (0x1E3F, "m")
      , (0x1E40, "M"), (0x1E41, "m"), (0x1E42, "M"), (0x1E43, "m")
      , (0x1E44, "N"), (0x1E45, "n"), (0x1E46, "N"), (0x1E47, "n")
      , (0x1E48, "N"), (0x1E49, "n"), (0x1E4A, "N"), (0x1E4B, "n")
      , (0x1E4C, "O"), (0x1E4D, "o"), (0x1E4E, "O"), (0x1E4F, "o")
      , (0x1E50, "O"), (0x1E51, "o"), (0x1E52, "O"), (0x1E53, "o")
      , (0x1E54, "P"), (0x1E55, "p"), (0x1E56, "P"), (0x1E57, "p")
      , (0x1E58, "R"), (0x1E59, "r"), (0x1E5A, "R"), (0x1E5B, "r")
      , (0x1E5C, "R"), (0x1E5D, "r"), (0x1E5E, "R"), (0x1E5F, "r")
      , (0x1E60, "S"), (0x1E61, "s"), (0x1E62, "S"), (0x1E63, "s")
      , (0x1E64, "S"), (0x1E65, "s"), (0x1E66, "S"), (0x1E67, "s")
      , (0x1E68, "S"), (0x1E69, "s"), (0x1E6A, "T"), (0x1E6B, "t")
      , (0x1E6C, "T"), (0x1E6D, "t"), (0x1E6E, "T"), (0x1E6F, "t")
      , (0x1E70, "T"), (0x1E71, "t"), (0x1E72, "U"), (0x1E73, "u")
      , (0x1E74, "U"), (0x1E75, "u"), (0x1E76, "U"), (0x1E77, "u")
      , (0x1E78, "U"), (0x1E79, "u"), (0x1E7A, "U"), (0x1E7B, "u")
      , (0x1E7C, "V"), (0x1E7D, "v"), (0x1E7E, "V"), (0x1E7F, "v")
      , (0x1E80, "W"), (0x1E81, "w"), (0x1E82, "W"), (0x1E83, "w")
      , (0x1E84, "W"), (0x1E85, "w"), (0x1E86, "W"), (0x1E87, "w")
      , (0x1E88, "W"), (0x1E89, "w"), (0x1E8A, "X"), (0x1E8B, "x")
      , (0x1E8C, "X"), (0x1E8D, "x"), (0x1E8E, "Y"), (0x1E8F, "y")
      , (0x1E90, "Z"), (0x1E91, "z"), (0x1E92, "Z"), (0x1E93, "z")
      , (0x1E94, "Z"), (0x1E95, "z"), (0x1E96, "h")
      , (0x1E97, "o"), (0x1E98, "w"), (0x1E99, "y")
      , (0x1E9A, "a"), (0x1E9B, "s")
      ]
