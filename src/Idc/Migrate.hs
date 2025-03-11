module Idc.Migrate
  ( runSchemaMigrations
  , ensureSeeded
  ) where

import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.Persist.Sql
import Idc.Config (Config(..))
import Idc.Import
import Idc.Models
import System.FilePath ((</>))

runSchemaMigrations :: ConnectionPool -> IO ()
runSchemaMigrations pool =
  runSqlPool (runMigration migrateAll) pool

ensureSeeded :: Config -> ConnectionPool -> IO ()
ensureSeeded cfg pool = do
  n <- runSqlPool (count ([] :: [Filter CatalogItem])) pool
  unless (n > 0) $ seedAll (cfgSeedDir cfg) pool

seedAll :: FilePath -> ConnectionPool -> IO ()
seedAll dir pool = do
  c10 <- parseCatalogCsv <$> TIO.readFile (dir </> "idc10.csv")
  c11 <- parseCatalogCsv <$> TIO.readFile (dir </> "idc11.csv")
  xw  <- parseCrosswalkCsv <$> TIO.readFile (dir </> "crosswalk.csv")
  runSqlPool (insertMany_ (toItems Idc10 c10 ++ toItems Idc11 c11 ++ toXw xw)) pool

toItems :: Catalog -> [Row] -> [CatalogItem]
toItems cat = map toItem
  where
    toItem (Row c p t e) =
      CatalogItem
        { catalogItemCatalog  = catalogTag cat
        , catalogItemCode     = c
        , catalogItemParent   = p
        , catalogItemTitle    = t
        , catalogItemChapter  = nonEmpty e
        , catalogItemLanguage = "en"
        }

nonEmpty :: T.Text -> Maybe T.Text
nonEmpty e = if T.null e then Nothing else Just e

toXw :: [(T.Text, T.Text, T.Text)] -> [Crosswalk]
toXw = map toOne
  where
    toOne (a, b, k) = Crosswalk a b k
