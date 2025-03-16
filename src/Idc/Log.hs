module Idc.Log
  ( LogEntry(..)
  , LogLevel(..)
  , logEntry
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

data LogLevel = Debug | Info | Warn | Error deriving (Show, Eq)

instance ToJSON LogLevel where
  toJSON Debug = "debug"
  toJSON Info  = "info"
  toJSON Warn  = "warn"
  toJSON Error = "error"

data LogEntry = LogEntry
  { leTimestamp :: UTCTime
  , leLevel     :: LogLevel
  , leMessage   :: Text
  , leRequestId :: Maybe Text
  } deriving (Show)

instance ToJSON LogEntry where
  toJSON e = object
    [ "timestamp" .= iso8601Show (leTimestamp e)
    , "level"     .= leLevel e
    , "message"   .= leMessage e
    , "requestId" .= leRequestId e
    ]

logEntry :: LogLevel -> Text -> Maybe Text -> IO LogEntry
logEntry lvl msg reqId = do
  ts <- getCurrentTime
  pure LogEntry { leTimestamp = ts, leLevel = lvl, leMessage = msg, leRequestId = reqId }