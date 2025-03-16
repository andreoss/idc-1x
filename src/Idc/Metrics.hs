module Idc.Metrics
  ( Metrics
  , mkMetrics
  , recordRequest
  , recordLatency
  , metricsMiddleware
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Network.Wai (Middleware)
import System.IO.Unsafe (unsafePerformIO)

data Metrics = Metrics
  { mRequestCount :: MVar Int
  , mLatencySum   :: MVar Int
  , mLatencyCount :: MVar Int
  }

mkMetrics :: IO Metrics
mkMetrics = Metrics <$> newMVar 0 <*> newMVar 0 <*> newMVar 0

recordRequest :: Metrics -> IO ()
recordRequest m = modifyMVar_ (mRequestCount m) (pure . (+1))

recordLatency :: Metrics -> Int -> IO ()
recordLatency m micros = do
  modifyMVar_ (mLatencySum m) (pure . (+ micros))
  modifyMVar_ (mLatencyCount m) (pure . (+1))

metricsMiddleware :: Metrics -> Middleware
metricsMiddleware m app req respond = do
  start <- getCurrentTime
  app req $ \resp -> do
    end <- getCurrentTime
    let micros = round (diffUTCTime end start * 1000000)
    recordRequest m
    recordLatency m micros
    respond resp

instance ToJSON Metrics where
  toJSON m = unsafePerformIO $ do
    requestCount <- readMVar (mRequestCount m)
    latencySum <- readMVar (mLatencySum m)
    latencyCount <- readMVar (mLatencyCount m)
    let avgLatency = if latencyCount > 0 then latencySum `div` latencyCount else 0
    pure $ object
      [ "requests_total" .= requestCount
      , "latency_us_sum" .= latencySum
      , "latency_us_count" .= latencyCount
      , "latency_us_avg" .= avgLatency
      ]