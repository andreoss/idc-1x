module Idc.App
  ( Env(..)
  , mkEnv
  ) where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)
import Idc.Cache (Cache, mkCache)
import Idc.Config (Config(..))
import Idc.Metrics (Metrics)

data Env = Env
  { envCfg    :: Config
  , envPool   :: ConnectionPool
  , envCache  :: Cache
  , envMetrics :: Metrics
  }

mkEnv :: Config -> Metrics -> IO Env
mkEnv cfg metrics = do
  pool <- runStdoutLoggingT $ createPostgresqlPool (encodeUtf8 (cfgPgConn cfg)) (cfgPoolSize cfg)
  cache <- mkCache
  pure Env { envCfg = cfg, envPool = pool, envCache = cache, envMetrics = metrics }
