module Idc.ImportSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Idc.Import
import Test.Hspec
import Test.QuickCheck

newtype SafeText = SafeText Text
  deriving (Eq, Show)

instance Arbitrary SafeText where
  arbitrary = do
    parts <- listOf1 (listOf1 (elements ['a' .. 'z']))
    pure (SafeText (T.intercalate " " (map T.pack parts)))

spec :: Spec
spec = do
  describe "validIdc10Code" $ do
    it "accepts chapter romans" $
      validIdc10Code "XIX" `shouldBe` True
    it "accepts block ranges" $
      validIdc10Code "A00-A09" `shouldBe` True
    it "accepts leaf codes" $ do
      validIdc10Code "A00" `shouldBe` True
      validIdc10Code "E11.9" `shouldBe` True
    it "rejects malformed codes" $ do
      validIdc10Code "a00" `shouldBe` False
      validIdc10Code "A000.1" `shouldBe` False
      validIdc10Code "12" `shouldBe` False

  describe "validIdc11Code" $ do
    it "accepts numeric chapters" $
      validIdc11Code "01" `shouldBe` True
    it "accepts stems and dotted leaves" $ do
      validIdc11Code "5A11" `shouldBe` True
      validIdc11Code "5A11.0" `shouldBe` True
    it "rejects bad shapes" $ do
      validIdc11Code "5A" `shouldBe` False
      validIdc11Code "5A111.22" `shouldBe` False

  describe "csv roundtrip" $
    it "renderRow after parseLine is identity for safe rows" $
      property $ \(SafeText c) (SafeText t) n ->
        let row = Row c (if n then Just "P1" else Nothing) t ""
            txt = renderRow row
        in parseLine txt == Just row

  describe "parseCatalogCsv" $
    it "skips header and malformed lines" $
      parseCatalogCsv "code,parent,title\nA00,,Cholera,junk extra\nBAD\n"
        `shouldMatchList`
          [Row "A00" Nothing "Cholera" "junk extra"]
