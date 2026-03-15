module Idc.ApiSpec (spec) where

import Data.Aeson (Value, decode, encode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Vector as V
import Idc.Api (PagedResponse(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "PagedResponse" $ do
    it "roundtrips through JSON" $ do
      let resp = mkResp "idc10" 1 10 100 Nothing Nothing Nothing Nothing 1 1 []
          decoded = decode (encode resp) :: Maybe PagedResponse
      show decoded `shouldBe` show (Just resp)

    it "preserves optional fields" $ do
      let resp = mkResp "idc11" 2 25 500 (Just "A") (Just "01") (Just 3) (Just 1) 1 20 []
          decoded = decode (encode resp) :: Maybe PagedResponse
      show decoded `shouldBe` show (Just resp)

    it "encodes catalog as text field" $ do
      let resp = mkResp "idc10" 1 10 100 Nothing Nothing Nothing Nothing 1 1 []
      case Aeson.toJSON resp of
        Aeson.Object o ->
          KM.lookup (K.fromText "catalog") o `shouldBe` Just (Aeson.String "idc10")
        _ -> expectationFailure "expected JSON object"

    it "encodes page numbers as integers" $ do
      let resp = mkResp "idc10" 3 25 200 Nothing Nothing Nothing Nothing 1 8 []
      case Aeson.toJSON resp of
        Aeson.Object o ->
          KM.lookup (K.fromText "page") o `shouldBe` Just (Aeson.Number 3)
        _ -> expectationFailure "expected JSON object"

    it "returns null for absent optional fields" $ do
      let resp = mkResp "idc10" 1 10 100 Nothing Nothing Nothing Nothing 1 1 []
      case Aeson.toJSON resp of
        Aeson.Object o -> do
          KM.lookup (K.fromText "parent") o `shouldBe` Just Aeson.Null
          KM.lookup (K.fromText "next") o `shouldBe` Just Aeson.Null
        _ -> expectationFailure "expected JSON object"

    it "includes all required keys" $ do
      let resp = mkResp "idc10" 1 10 100 Nothing Nothing Nothing Nothing 1 1 []
          keys = ["catalog", "page", "perPage", "total", "first", "last", "items"] :: [Text]
      case Aeson.toJSON resp of
        Aeson.Object o ->
          mapM_ (\k -> KM.lookup (K.fromText k) o `shouldSatisfy` (/= Nothing)) keys
        _ -> expectationFailure "expected JSON object"

    it "encodes parent when present" $ do
      let resp = mkResp "idc10" 1 10 100 (Just "A01") Nothing Nothing Nothing 1 1 []
      case Aeson.toJSON resp of
        Aeson.Object o ->
          KM.lookup (K.fromText "parent") o `shouldBe` Just (Aeson.String "A01")
        _ -> expectationFailure "expected JSON object"

    it "encodes items as empty array when no results" $ do
      let resp = mkResp "idc10" 1 10 0 Nothing Nothing Nothing Nothing 1 1 []
      case Aeson.toJSON resp of
        Aeson.Object o ->
          KM.lookup (K.fromText "items") o `shouldBe` Just (Aeson.Array V.empty)
        _ -> expectationFailure "expected JSON object"

    it "JSON contains all required field names" $ do
      let resp = mkResp "idc10" 1 10 100 Nothing Nothing Nothing Nothing 1 1 []
          json = show (encode resp)
      json `shouldSatisfy` \s -> "catalog" `isInfixOf` s
      json `shouldSatisfy` \s -> "page" `isInfixOf` s
      json `shouldSatisfy` \s -> "perPage" `isInfixOf` s
      json `shouldSatisfy` \s -> "total" `isInfixOf` s

mkResp :: Text -> Int -> Int -> Int -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Int -> Int -> [Value] -> PagedResponse
mkResp cat pg pp t p ch nx pv f l items = PagedResponse
  { prCatalog  = cat
  , prPage     = pg
  , prPerPage  = pp
  , prTotal    = t
  , prParent   = p
  , prChapter  = ch
  , prNext     = nx
  , prPrev     = pv
  , prFirst    = f
  , prLast     = l
  , prItems    = items
  }
