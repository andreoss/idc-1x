module Idc.Models
  ( Catalog(..)
  , MapKind(..)
  , CatalogItem(..)
  , Crosswalk(..)
  , catalogTag
  , catalogFromTag
  , mapKindToText
  , mapKindFromText
  , migrateAll
  ) where

import Data.Text (Text)
import Database.Persist.TH

data Catalog = Idc10 | Idc11
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data MapKind = Equal | Narrower | Broader | Related
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CatalogItem
    itemCatalog Text
    itemCode Text
    itemParent Text Maybe
    itemTitle Text
    itemChapter Text Maybe
    itemLanguage Text
    UniqueItem itemCatalog itemCode itemLanguage
    deriving Show Eq

Crosswalk
    cwIcd10 Text
    cwIcd11 Text
    cwKind Text
    UniquePair cwIcd10 cwIcd11
    deriving Show Eq
|]

catalogTag :: Catalog -> Text
catalogTag Idc10 = "idc10"
catalogTag Idc11 = "idc11"

catalogFromTag :: Text -> Maybe Catalog
catalogFromTag "idc10" = Just Idc10
catalogFromTag "idc11" = Just Idc11
catalogFromTag _       = Nothing

mapKindToText :: MapKind -> Text
mapKindToText Equal    = "equal"
mapKindToText Narrower = "narrower"
mapKindToText Broader  = "broader"
mapKindToText Related  = "related"

mapKindFromText :: Text -> MapKind
mapKindFromText "equal"    = Equal
mapKindFromText "narrower" = Narrower
mapKindFromText "broader"  = Broader
mapKindFromText _          = Related
