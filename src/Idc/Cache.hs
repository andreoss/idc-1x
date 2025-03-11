module Idc.Cache
  ( Cache(..)
  , mkNullCache
  , mkRedisCache
  , parseRedisUrl
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Database.Redis as R

data Cache = Cache
  { cGet   :: Text -> IO (Maybe BL.ByteString)
  , cSet   :: Text -> BL.ByteString -> IO ()
  , cDel   :: Text -> IO ()
  , cPurge :: IO ()
  }

mkNullCache :: Cache
mkNullCache = Cache (\_ -> pure Nothing) (\_ _ -> pure ()) (\_ -> pure ()) (pure ())

parseRedisUrl :: Text -> R.ConnectInfo
parseRedisUrl url =
  let stripped = fromMaybe' (T.stripPrefix "redis://" url) url
      parts    = T.splitOn ":" stripped
      host     = if T.null stripped then "127.0.0.1" else T.unpack (head parts)
      portPart = if length parts > 1 then T.unpack (parts !! 1) else "6379"
  in R.defaultConnectInfo
       { R.connectHost = host
       , R.connectPort = R.PortNumber (fromIntegral (readPort portPart))
       }

readPort :: String -> Int
readPort s = case reads s of
  [(n, "")]  -> n
  _          -> 6379

fromMaybe' :: Maybe a -> a -> a
fromMaybe' m d = maybe d id m

mkRedisCache :: R.ConnectInfo -> Int -> IO Cache
mkRedisCache ci ttlSec = do
  conn <- R.checkedConnect ci
  pure Cache
    { cGet = \k -> do
        r <- R.runRedis conn (R.get (encodeUtf8 k))
        pure (either (const Nothing) (fmap BL.fromStrict) r)
    , cSet = \k v -> do
        _ <- R.runRedis conn (R.setex (encodeUtf8 k) (toInteger ttlSec) (BL.toStrict v))
        pure ()
    , cDel = \k -> do
        _ <- R.runRedis conn (R.del [encodeUtf8 k])
        pure ()
    , cPurge = do
        _ <- R.runRedis conn (R.flushdb)
        pure ()
    }
