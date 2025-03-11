module Idc.Search
  ( Hit(..)
  , scoreHit
  , rankHits
  ) where

import Data.Foldable (foldr')
import Data.Ord (comparing)
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
      | f y < f x || (f y == f x && cmpIdx x y == LT) = x : y : ys
      | otherwise = y : insert x ys
    cmpIdx x y = comparing fst x y
