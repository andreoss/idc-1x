module Idc.Cache
  ( Cache(..)
  , mkCache
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data Cache = Cache
  { cGet   :: Text -> IO (Maybe BL.ByteString)
  , cSet   :: Text -> BL.ByteString -> IO ()
  , cDel   :: Text -> IO ()
  , cPurge :: IO ()
  }

mkCache :: IO Cache
mkCache = do
  ref <- newIORef Map.empty
  pure Cache
    { cGet = \k -> Map.lookup k <$> readIORef ref
    , cSet = \k v -> modifyIORef' ref (Map.insert k v)
    , cDel = \k -> modifyIORef' ref (Map.delete k)
    , cPurge = writeIORef ref Map.empty
    }
