module Main (main) where

import qualified Idc.ImportSpec
import qualified Idc.SearchSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Import" Idc.ImportSpec.spec
  describe "Search" Idc.SearchSpec.spec
