module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString, word64Hex)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word64)
import Idc.App (Env(..), mkEnv, envPool)
import Idc.Api (idcApi, idcServer)
import Idc.Config (cfgPort, loadConfig, validateConfig)
import Idc.Migrate (ensureSeeded, runSchemaMigrations)
import Network.Wai (Application, Middleware, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, defaultGzipSettings)
import Servant (serve)
import System.Random (randomIO)

requestIdMiddleware :: Middleware
requestIdMiddleware app req respond = do
  rid <- generateRequestId
  let req' = req { requestHeaders = ("x-request-id", rid) : requestHeaders req }
  app req' respond

generateRequestId :: IO ByteString
generateRequestId = do
  bs <- randomIO :: IO Word64
  pure $ BL.toStrict (Data.ByteString.Builder.toLazyByteString (Data.ByteString.Builder.word64Hex bs))

application :: Env -> Application
application env = gzip defaultGzipSettings (requestIdMiddleware (serve idcApi (idcServer env)))

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
