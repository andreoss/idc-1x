module Idc.Log
  ( LogEntry(..)
  , LogLevel(..)
  , logEntry
  , requestLogMiddleware
  ) where

import Data.Aeson (ToJSON(..), encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Network.Wai (Middleware, rawPathInfo, requestHeaders, requestMethod)

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

requestLogMiddleware :: Middleware
requestLogMiddleware app req respond = do
  let reqId = TE.decodeUtf8 <$> lookup "x-request-id" (requestHeaders req)
      msg = TE.decodeUtf8 (requestMethod req) <> " " <> TE.decodeUtf8 (rawPathInfo req)
  entry <- logEntry Info msg reqId
  BSL8.putStrLn (encode entry)
  app req respond