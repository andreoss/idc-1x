module Idc.SearchSpec (spec) where

import Data.Text (Text)
import Idc.Search
import Test.Hspec

hit :: Text -> Text -> Hit
hit = Hit

spec :: Spec
spec = do
  describe "scoreHit" $ do
    it "rewards exact title matches highest" $
      scoreHit "cholera" (hit "A00" "Cholera")
        > scoreHit "cholera" (hit "A09" "Infectious gastroenteritis and colitis")
        `shouldBe` True

    it "boosts code prefix hits" $
      scoreHit "E11" (hit "E11" "Type 2 diabetes mellitus")
        > scoreHit "E11" (hit "Z99" "Unrelated entry")
        `shouldBe` True

  describe "rankHits" $ do
    it "orders best first" $
      rankHits "asthma"
        [ hit "J06" "Acute upper respiratory infections"
        , hit "J45" "Asthma"
        , hit "J45.9" "Asthma unspecified"
        ]
        `shouldBe`
          [ hit "J45" "Asthma"
          , hit "J45.9" "Asthma unspecified"
          , hit "J06" "Acute upper respiratory infections"
          ]

    it "is stable for equal scores" $
      length (rankHits "zzz" [hit "A00" "x", hit "B01" "y"]) `shouldBe` 2

  describe "normalizeText" $ do
    it "folds German umlauts" $
      normalizeText "über" `shouldBe` "uber"

    it "folds French accented chars" $
      normalizeText "café" `shouldBe` "cafe"

    it "folds Spanish n-tilde" $
      normalizeText "niño" `shouldBe` "nino"

    it "folds uppercase diacritics" $
      normalizeText "ÉNTRY" `shouldBe` "ENTRY"

    it "preserves plain ASCII" $
      normalizeText "hello" `shouldBe` "hello"

    it "folds Latin Extended-A block" $
      normalizeText "čšž" `shouldBe` "csz"

    it "folds ogonek and cedilla" $
      normalizeText "ąęçş" `shouldBe` "aecs"

    it "folds Polish l-stroke" $
      normalizeText "łódź" `shouldBe` "lodz"
