module Idc.Api
  ( IdcApi
  , idcApi
  , idcServer
  , swaggerDoc
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Reader (mapReaderT)
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Swagger (Swagger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.Esqueleto.Legacy hiding (Value, runSqlPool, count)
import Database.Persist.Sql (Filter, count, runSqlPool)
import Servant

import Idc.App (Env(..))
import Idc.Models
import Idc.Search

type IdcApi =
       "healthz" :> Get '[JSON] Value
  :<|> "swagger.json" :> Get '[JSON] Swagger
  :<|> "api" :> "v1" :> CatalogApi

type CatalogApi =
       Capture "catalog" Text :> "items"
         :> QueryParam "page" Int
         :> QueryParam "perPage" Int
         :> QueryParam "parent" Text
         :> Get '[JSON] Value
  :<|> Capture "catalog" Text :> "items" :> Capture "code" Text
         :> Get '[JSON] Value
  :<|> Capture "catalog" Text :> "search"
         :> QueryParam "q" Text
         :> QueryParam "limit" Int
         :> Get '[JSON] Value
  :<|> "crosswalk"
         :> QueryParam "icd10" Text
         :> QueryParam "icd11" Text
         :> Get '[JSON] Value

idcApi :: Proxy IdcApi
idcApi = Proxy

swaggerDoc :: Swagger
swaggerDoc = mempty

idcServer :: Env -> Server IdcApi
idcServer env =
       healthH
  :<|> pure swaggerDoc
  :<|> catalogH env

healthH :: Handler Value
healthH = pure (object ["status" .= ("ok" :: Text)])

catalogH :: Env -> Server CatalogApi
catalogH env =
       listItems env
  :<|> getItem env
  :<|> searchItems env
  :<|> crosswalkH env

resolveCatalog :: Text -> Handler Catalog
resolveCatalog tag =
  maybe (failWith err404 "unknown catalog") pure (catalogFromTag tag)

failWith :: ServerError -> Text -> Handler a
failWith e t = throwError e { errBody = BL.fromStrict (encodeUtf8 t) }

db :: Env -> SqlPersistM a -> IO a
db env act = runSqlPool (mapReaderT (runResourceT . runNoLoggingT) act) (envPool env)

listItems :: Env -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Handler Value
listItems env tag mpage mperPage mparent = do
  cat <- resolveCatalog tag
  let page    = max 1 (fromMaybe 1 mpage)
      perPage = clampP (fromMaybe 25 mperPage)
      offset' = (page - 1) * perPage
      itemFilter i = do
        where_ (i ^. CatalogItemCatalog ==. val (catalogTag cat))
        case mparent of
          Just p  -> where_ (i ^. CatalogItemParent ==. just (val p))
          Nothing -> pure ()
  total <- liftIO $ db env $ do
    cnt <- select $ from $ \i -> itemFilter i >> pure countRows
    pure (maybe 0 unValue (listToMaybe cnt))
  rows <- liftIO $ db env $ select $
    from $ \i -> do
      itemFilter i
      orderBy [asc (i ^. CatalogItemCode)]
      limit (fromIntegral perPage)
      offset (fromIntegral offset')
      pure i
  pure $ object
    [ "catalog" .= catalogTag cat
    , "page" .= page
    , "perPage" .= perPage
    , "total" .= total
    , "parent" .= mparent
    , "items" .= map itemJson rows
    ]

clampP :: Int -> Int
clampP = max 1 . min 200

getItem :: Env -> Text -> Text -> Handler Value
getItem env tag code = do
  cat <- resolveCatalog tag
  results <- liftIO $ db env $ select $
    from $ \i -> do
      where_ (i ^. CatalogItemCatalog ==. val (catalogTag cat)
              &&. i ^. CatalogItemCode ==. val code)
      limit 1
      pure i
  case results of
    []    -> failWith err404 "code not found"
    (e:_) -> pure (itemJson e)

searchItems :: Env -> Text -> Maybe Text -> Maybe Int -> Handler Value
searchItems env tag mq mlimit = do
  cat <- resolveCatalog tag
  q <- case fmap T.strip mq of
    Nothing      -> failWith err400 "missing q"
    Just ""      -> failWith err400 "empty q"
    Just x       -> pure x
  let lim = max 1 (min 50 (fromMaybe 10 mlimit))
      pat = T.concat ["%", T.toLower q, "%"]
      qpat = T.concat [T.toLower q, "%"]
  raw <- liftIO $ db env $ select $
    from $ \i -> do
      where_ (i ^. CatalogItemCatalog ==. val (catalogTag cat)
              &&. ((lower_ (i ^. CatalogItemTitle) `like` val pat)
                   ||. (i ^. CatalogItemCode `like` val qpat)))
      limit 500
      pure i
  let hits = rankHits q (map toHit raw)
  pure $ object
    [ "catalog" .= catalogTag cat
    , "q" .= q
    , "hits" .= take lim (map hitJson hits)
    ]

toHit :: Entity CatalogItem -> Hit
toHit e = Hit (catalogItemCode v) (catalogItemTitle v)
  where v = entityVal e

hitJson :: Hit -> Value
hitJson h = object ["code" .= hitCode h, "title" .= hitTitle h]

itemJson :: Entity CatalogItem -> Value
itemJson e =
  let v = entityVal e
  in object
       [ "code" .= catalogItemCode v
       , "parent" .= catalogItemParent v
       , "title" .= catalogItemTitle v
       , "chapter" .= catalogItemChapter v
       , "language" .= catalogItemLanguage v
       ]

crosswalkH :: Env -> Maybe Text -> Maybe Text -> Handler Value
crosswalkH env ma mb = do
  rows <- liftIO $ db env $ select $
    from $ \x -> do
      case (ma, mb) of
        (Just a, Just b) ->
          where_ (x ^. CrosswalkIcd10 ==. val a &&. x ^. CrosswalkIcd11 ==. val b)
        (Just a, Nothing) ->
          where_ (x ^. CrosswalkIcd10 ==. val a)
        (Nothing, Just b) ->
          where_ (x ^. CrosswalkIcd11 ==. val b)
        (Nothing, Nothing) ->
          pure ()
      limit 1000
      pure x
  pure $ object ["mappings" .= map xwJson rows]

xwJson :: Entity Crosswalk -> Value
xwJson e =
  let v = entityVal e
  in object
       [ "icd10" .= crosswalkIcd10 v
       , "icd11" .= crosswalkIcd11 v
       , "kind" .= crosswalkKind v
       ]
