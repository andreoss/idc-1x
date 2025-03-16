module Idc.Cors
  ( corsMiddleware
  ) where

import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.Wai (Middleware, requestHeaders, requestMethod, responseLBS, mapResponseHeaders)
import qualified Data.Text.Encoding as TE

corsMiddleware :: [Text] -> Middleware
corsMiddleware allowedOrigins app req respond = do
  let origin = lookup "origin" (requestHeaders req)
      allowOrigin = case origin of
        Just o | TE.decodeUtf8 o `elem` allowedOrigins -> o
        _ -> if null allowedOrigins then TE.encodeUtf8 "*" else TE.encodeUtf8 (head allowedOrigins)
      headers = [ ("Access-Control-Allow-Origin", allowOrigin)
                , ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
                , ("Access-Control-Allow-Headers", "Content-Type, Authorization, X-Request-ID")
                , ("Access-Control-Max-Age", "86400")
                ]
  if requestMethod req == "OPTIONS"
    then respond $ responseLBS status200 headers ""
    else app req $ \resp -> respond $ mapResponseHeaders (++ headers) resp