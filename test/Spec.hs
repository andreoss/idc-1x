module Main (main) where

import qualified Idc.ApiSpec
import qualified Idc.ImportSpec
import qualified Idc.ProblemSpec
import qualified Idc.SearchSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Api" Idc.ApiSpec.spec
  describe "Import" Idc.ImportSpec.spec
  describe "Problem" Idc.ProblemSpec.spec
  describe "Search" Idc.SearchSpec.spec
