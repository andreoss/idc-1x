module Idc.Config
  ( Config(..)
  , loadConfig
  , validateConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

data Config = Config
  { cfgPgConn    :: Text
  , cfgPort      :: Int
  , cfgSeedDir   :: FilePath
  , cfgPoolSize  :: Int
  , cfgLogLevel  :: Text
  , cfgFeatures  :: FeatureFlags
  } deriving (Show)

data FeatureFlags = FeatureFlags
  { flagEnableCache   :: Bool
  , flagEnableMetrics :: Bool
  } deriving (Show)

defaultFlags :: FeatureFlags
defaultFlags = FeatureFlags
  { flagEnableCache   = True
  , flagEnableMetrics = False
  }

loadConfig :: IO Config
loadConfig =
  Config
    <$> reqText "PG_CONN"
    <*> (maybe 8080 read <$> lookupEnv "PORT")
    <*> (maybe "seed" id <$> lookupEnv "SEED_DIR")
    <*> (maybe 10 read <$> lookupEnv "POOL_SIZE")
    <*> (maybe "info" id <$> lookupEnv "LOG_LEVEL")
    <*> (featureFlagsFromEnv <$> lookupEnv "ENABLE_CACHE" <*> lookupEnv "ENABLE_METRICS")

featureFlagsFromEnv :: Maybe String -> Maybe String -> FeatureFlags
featureFlagsFromEnv mCache mMetrics = FeatureFlags
  { flagEnableCache   = parseBool mCache True
  , flagEnableMetrics = parseBool mMetrics False
  }

parseBool :: Maybe String -> Bool -> Bool
parseBool Nothing def = def
parseBool (Just "true") _ = True
parseBool (Just "false") _ = False
parseBool _ def = def

reqText :: String -> IO Text
reqText k = do
  v <- lookupEnv k
  case v of
    Nothing -> fail ("missing env " ++ k)
    Just s  -> pure (T.pack s)

validateConfig :: Config -> Either String ()
validateConfig cfg
  | T.null (cfgPgConn cfg) = Left "PG_CONN must not be empty"
  | cfgPort cfg < 1 || cfgPort cfg > 65535 = Left "PORT must be between 1 and 65535"
  | otherwise = Right ()
