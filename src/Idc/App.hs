module Idc.App
  ( Env(..)
  , mkEnv
  ) where

import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)
import Idc.Cache (Cache, mkCache)
import Idc.Config (Config(..))

data Env = Env
  { envCfg   :: Config
  , envPool  :: ConnectionPool
  , envCache :: Cache
  }

mkEnv :: Config -> IO Env
mkEnv cfg = do
  pool <- createPostgresqlPool (encodeUtf8 (cfgPgConn cfg)) 10
  cache <- mkCache
  pure (Env cfg pool cache)
