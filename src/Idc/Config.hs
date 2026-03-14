module Idc.Config
  ( Config(..)
  , loadConfig
  , validateConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

data Config = Config
  { cfgPgConn  :: Text
  , cfgPort    :: Int
  , cfgSeedDir :: FilePath
  } deriving (Show)

loadConfig :: IO Config
loadConfig =
  Config
    <$> reqText "PG_CONN"
    <*> (maybe 8080 read <$> lookupEnv "PORT")
    <*> (maybe "seed" id <$> lookupEnv "SEED_DIR")

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
