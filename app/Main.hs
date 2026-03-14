module Main (main) where

import Idc.App (Env(..), mkEnv, envPool)
import Idc.Api (idcApi, idcServer)
import Idc.Config (cfgPort, loadConfig, validateConfig)
import Idc.Migrate (ensureSeeded, runSchemaMigrations)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (serve)

application :: Env -> Application
application env = serve idcApi (idcServer env)

main :: IO ()
main = do
  cfg <- loadConfig
  case validateConfig cfg of
    Left err -> fail err
    Right () -> pure ()
  env <- mkEnv cfg
  runSchemaMigrations (envPool env)
  ensureSeeded cfg (envPool env)
  run (cfgPort cfg) (application env)
