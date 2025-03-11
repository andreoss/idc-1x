module Main (main) where

import Idc.App (application, mkEnv, envPool)
import Idc.Config (cfgPort, loadConfig)
import Idc.Migrate (ensureSeeded, runSchemaMigrations)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  cfg <- loadConfig
  env <- mkEnv cfg
  runSchemaMigrations (envPool env)
  ensureSeeded cfg (envPool env)
  run (cfgPort cfg) (application env)
