module Idc.Security
  ( securityHeadersMiddleware
  ) where

import Data.ByteString (ByteString)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai (Middleware, mapResponseHeaders)

securityHeaders :: [(HeaderName, ByteString)]
securityHeaders =
  [ ("X-Content-Type-Options", "nosniff")
  , ("X-Frame-Options", "DENY")
  , ("X-XSS-Protection", "1; mode=block")
  , ("Content-Security-Policy", "default-src 'none'; frame-ancestors 'none'; base-uri 'none'; form-action 'none'")
  , ("Referrer-Policy", "no-referrer")
  , ("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
  ]

securityHeadersMiddleware :: Middleware
securityHeadersMiddleware app req respond = do
  let addHeaders resp = mapResponseHeaders (++ securityHeaders) resp
  app req (respond . addHeaders)