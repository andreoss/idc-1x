module Idc.App
  ( Env(..)
  , mkEnv
  , application
  ) where

import qualified Data.Text as T
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)
import Idc.Api (idcApi, idcServer)
import Idc.Cache (Cache, mkNullCache, parseRedisUrl, mkRedisCache)
import Idc.Config (Config(..))
import Network.Wai (Application)
import Servant (serve)

data Env = Env
  { envCfg   :: Config
  , envPool  :: ConnectionPool
  , envCache :: Cache
  }

mkEnv :: Config -> IO Env
mkEnv cfg = do
  pool <- createPostgresqlPool (T.unpack (cfgPgConn cfg)) 10
  cache <- case cfgRedisUrl cfg of
    Nothing -> pure mkNullCache
    Just u  -> mkRedisCache (parseRedisUrl u) 300
  pure (Env cfg pool cache)

application :: Env -> Application
application env = serve idcApi (idcServer env)
