module Idc.Problem
  ( Problem(..)
  , problemHandler
  ) where

import Data.Aeson (FromJSON, ToJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Servant (ServerError(..))

data Problem = Problem
  { problemType     :: Text
  , problemTitle    :: Text
  , problemStatus   :: Int
  , problemDetail   :: Text
  , problemInstance :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON Problem where
  toJSON p = Aeson.object
    [ "type"     .= problemType p
    , "title"    .= problemTitle p
    , "status"   .= problemStatus p
    , "detail"   .= problemDetail p
    , "instance" .= problemInstance p
    ]

instance FromJSON Problem where
  parseJSON = Aeson.withObject "Problem" $ \o ->
    Problem
      <$> o .: "type"
      <*> o .: "title"
      <*> o .: "status"
      <*> o .: "detail"
      <*> o .:? "instance"

problemHandler :: ServerError -> Text -> ServerError
problemHandler err detail = err
  { errBody = Aeson.encode (Problem
      { problemType     = "about:blank"
      , problemTitle    = englishTitle (errHTTPCode err)
      , problemStatus   = errHTTPCode err
      , problemDetail   = detail
      , problemInstance = Nothing
      })
  , errHeaders = [("Content-Type", "application/problem+json")]
  }

englishTitle :: Int -> Text
englishTitle 400 = "Bad Request"
englishTitle 404 = "Not Found"
englishTitle 500 = "Internal Server Error"
englishTitle n   = "Error " <> T.pack (show n)
