module Idc.RateLimit
  ( RateLimiter
  , mkRateLimiter
  , checkRate
  , rateLimitMiddleware
  ) where

import qualified Data.Aeson as Aeson
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Idc.Problem (Problem(..))
import Network.HTTP.Types (status429)
import Network.Wai (Middleware, remoteHost, responseLBS)

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

rateLimitMiddleware :: RateLimiter -> Middleware
rateLimitMiddleware rl app req respond = do
  let key = T.pack (show (remoteHost req))
  allowed <- checkRate rl key
  if allowed
    then app req respond
    else respond $ responseLBS status429
      [("Content-Type", "application/problem+json")]
      (Aeson.encode (Problem "about:blank" "Too Many Requests" 429 "rate limit exceeded" Nothing))