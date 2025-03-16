module Idc.RateLimit
  ( RateLimiter
  , mkRateLimiter
  , checkRate
  ) where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

data RateLimiter = RateLimiter
  { rlRef     :: IORef (Map Text [(UTCTime, Int)])
  , rlLimit   :: Int
  , rlWindow  :: Int
  }

mkRateLimiter :: Int -> Int -> IO RateLimiter
mkRateLimiter limit window = do
  ref <- newIORef Map.empty
  pure RateLimiter
    { rlRef = ref
    , rlLimit = limit
    , rlWindow = window
    }

checkRate :: RateLimiter -> Text -> IO Bool
checkRate rl key = do
  now <- getCurrentTime
  let window = rlWindow rl
      limit = rlLimit rl
  atomicModifyIORef' (rlRef rl) $ \m ->
    case Map.lookup key m of
      Nothing ->
        let m' = Map.insert key [(now, 1)] m
        in (m', True)
      Just hitsList ->
        let valid = filter (\(t, _) -> diffUTCTime now t < fromIntegral window) hitsList
            total = sum (map snd valid)
        in if total >= limit
           then (m, False)
           else let newHits = (now, 1) : valid
                    m' = Map.insert key newHits m
                in (m', True)