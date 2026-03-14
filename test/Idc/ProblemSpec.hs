module Idc.ProblemSpec (spec) where

import Data.Aeson (decode, encode)
import Idc.Problem
import Test.Hspec

spec :: Spec
spec = do
  describe "Problem JSON" $ do
    it "serializes all fields" $ do
      let p = Problem "about:blank" "Not Found" 404 "unknown catalog" Nothing
          j = encode p
      decode j `shouldBe` Just p

    it "roundtrips with instance field" $ do
      let p = Problem "https://example.com/errors/validation" "Bad Request" 400 "invalid input" (Just "/api/v1/items")
      decode (encode p) `shouldBe` Just p

    it "roundtrips with null instance" $ do
      let p = Problem "about:blank" "Not Found" 404 "code not found" Nothing
      decode (encode p) `shouldBe` Just p

    it "includes all required RFC 7807 fields" $ do
      let p = Problem "about:blank" "Error" 500 "fail" Nothing
          j = encode p
      decode j `shouldBe` Just p
      case decode j :: Maybe Problem of
        Nothing -> expectationFailure "failed to decode"
        Just decoded -> do
          problemType decoded `shouldBe` "about:blank"
          problemTitle decoded `shouldBe` "Error"
          problemStatus decoded `shouldBe` 500
          problemDetail decoded `shouldBe` "fail"
          problemInstance decoded `shouldBe` Nothing
