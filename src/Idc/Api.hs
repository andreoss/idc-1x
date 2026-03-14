module Idc.Api
  ( IdcApi
  , idcApi
  , idcServer
  , swaggerDoc
  , PagedResponse(..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Reader (mapReaderT)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Swagger (Swagger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.Esqueleto.Legacy hiding (Value, runSqlPool, count)
import Database.Persist.Sql (Filter, count, runSqlPool)
import Servant
import System.FilePath ((</>))

import Idc.App (Env(..))
import Idc.Import (ImportResult(..), parseCatalogCsv, parseCrosswalkCsv)
import Idc.Models
import Idc.Problem (Problem(..), throwProblem)
import Idc.Search

data PagedResponse = PagedResponse
  { prCatalog  :: Text
  , prPage     :: Int
  , prPerPage  :: Int
  , prTotal    :: Int
  , prParent   :: Maybe Text
  , prChapter  :: Maybe Text
  , prNext     :: Maybe Int
  , prPrev     :: Maybe Int
  , prFirst    :: Int
  , prLast     :: Int
  , prItems    :: [Value]
  } deriving (Show)

instance Aeson.ToJSON PagedResponse where
  toJSON p = object
    [ "catalog"  .= prCatalog p
    , "page"     .= prPage p
    , "perPage"  .= prPerPage p
    , "total"    .= prTotal p
    , "parent"   .= prParent p
    , "chapter"  .= prChapter p
    , "next"     .= prNext p
    , "prev"     .= prPrev p
    , "first"    .= prFirst p
    , "last"     .= prLast p
    , "items"    .= prItems p
    ]

instance Aeson.FromJSON PagedResponse where
  parseJSON = Aeson.withObject "PagedResponse" $ \o ->
    PagedResponse
    <$> o Aeson..:  "catalog"
    <*> o Aeson..:  "page"
    <*> o Aeson..:  "perPage"
    <*> o Aeson..:  "total"
    <*> o Aeson..:? "parent"
    <*> o Aeson..:? "chapter"
    <*> o Aeson..:? "next"
    <*> o Aeson..:? "prev"
    <*> o Aeson..:  "first"
    <*> o Aeson..:  "last"
    <*> o Aeson..:  "items"

newtype ImportRequest = ImportRequest
  { seedDir :: FilePath
  } deriving (Eq, Show)

instance Aeson.ToJSON ImportRequest where
  toJSON (ImportRequest d) = Aeson.object ["seedDir" Aeson..= d]

instance Aeson.FromJSON ImportRequest where
  parseJSON = Aeson.withObject "ImportRequest" $ \o ->
    ImportRequest <$> o Aeson..: "seedDir"

type IdcApi =
       "healthz" :> Get '[JSON] Value
  :<|> "readyz" :> Get '[JSON] Value
  :<|> "swagger.json" :> Get '[JSON] Swagger
  :<|> "admin" :> "import" :> ReqBody '[JSON] ImportRequest
         :> Post '[JSON] Value
  :<|> "api" :> "v1" :> CatalogApi

type CatalogApi =
       Capture "catalog" Text :> "items"
         :> QueryParam "page" Int
         :> QueryParam "perPage" Int
         :> QueryParam "parent" Text
         :> QueryParam "chapter" Text
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
       healthH env
  :<|> readyzH env
  :<|> pure swaggerDoc
  :<|> importH
  :<|> catalogH env

healthH :: Env -> Handler Value
healthH env = do
  dbOk <- liftIO $ do
    result <- try (db env $ rawExecute "SELECT 1" [])
    pure $ case result of
      Left (_ :: SomeException) -> False
      Right _                   -> True
  let status = if dbOk then ("ok" :: Text) else ("degraded" :: Text)
      dbStat = if dbOk then ("ok" :: Text) else ("error" :: Text)
  pure (object ["status" .= status, "db" .= dbStat])

readyzH :: Env -> Handler Value
readyzH env = do
  dbOk <- liftIO $ do
    result <- try (db env $ rawExecute "SELECT 1" [])
    pure $ case result of
      Left (_ :: SomeException) -> False
      Right _                   -> True
  if dbOk
    then pure (object ["status" .= ("ok" :: Text)])
    else throwError err503 { errBody = Aeson.encode (object ["status" .= ("not ready" :: Text)]), errHeaders = [("Content-Type", "application/json")] }

catalogH :: Env -> Server CatalogApi
catalogH env =
       listItems env
  :<|> getItem env
  :<|> searchItems env
  :<|> crosswalkH env

resolveCatalog :: Text -> Handler Catalog
resolveCatalog tag =
  maybe (throwProblem (Problem "about:blank" "Not Found" 404 "unknown catalog" Nothing)) pure (catalogFromTag tag)

importH :: ImportRequest -> Handler Value
importH req = do
  let dir = seedDir req
  c10 <- liftIO $ parseCatalogCsv <$> TIO.readFile (dir </> "idc10.csv")
  c11 <- liftIO $ parseCatalogCsv <$> TIO.readFile (dir </> "idc11.csv")
  xw  <- liftIO $ parseCrosswalkCsv <$> TIO.readFile (dir </> "crosswalk.csv")
  let allErrs = irErrors c10 ++ irErrors c11
      totalRows = length (irRows c10) + length (irRows c11) + length xw
  pure $ object
    [ "errors" .= allErrs
    , "catalogItems" .= (length (irRows c10) + length (irRows c11))
    , "crosswalkRows" .= length xw
    , "totalParsed" .= totalRows
    ]

db :: Env -> SqlPersistM a -> IO a
db env act = runSqlPool (mapReaderT (runResourceT . runNoLoggingT) act) (envPool env)

listItems :: Env -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler Value
listItems env tag mpage mperPage mparent mchapter = do
  cat <- resolveCatalog tag
  let page    = max 1 (fromMaybe 1 mpage)
      perPage = clampP (fromMaybe 25 mperPage)
      offset' = (page - 1) * perPage
      itemFilter i = do
        where_ (i ^. CatalogItemCatalog ==. val (catalogTag cat))
        case mparent of
          Just p  -> where_ (i ^. CatalogItemParent ==. just (val p))
          Nothing -> pure ()
        case mchapter of
          Just ch -> where_ (i ^. CatalogItemChapter ==. just (val ch))
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
  let totalPages = max 1 (ceiling (fromIntegral total / fromIntegral perPage :: Double) :: Int)
      nxt = if page < totalPages then Just (page + 1) else Nothing
      prv = if page > 1 then Just (page - 1) else Nothing
      resp = PagedResponse
        { prCatalog  = catalogTag cat
        , prPage     = page
        , prPerPage  = perPage
        , prTotal    = total
        , prParent   = mparent
        , prChapter  = mchapter
        , prNext     = nxt
        , prPrev     = prv
        , prFirst    = 1
        , prLast     = totalPages
        , prItems    = map itemJson rows
        }
  pure (Aeson.toJSON resp)

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
    []    -> throwProblem (Problem "about:blank" "Not Found" 404 "code not found" Nothing)
    (e:_) -> pure (itemJson e)

searchItems :: Env -> Text -> Maybe Text -> Maybe Int -> Handler Value
searchItems env tag mq mlimit = do
  cat <- resolveCatalog tag
  q <- case fmap T.strip mq of
    Nothing      -> throwProblem (Problem "about:blank" "Bad Request" 400 "missing q" Nothing)
    Just ""      -> throwProblem (Problem "about:blank" "Bad Request" 400 "empty q" Nothing)
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
