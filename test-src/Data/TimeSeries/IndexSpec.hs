module Data.TimeSeries.IndexSpec (spec) where

import Prelude
import Test.Hspec
import Test.QuickCheck

import Data.TimeSeries.Index


spec :: Spec
spec =

  describe "Build index" $ do

    it "from timestamps" $ do
        let idx = fromTimestamps [1, 2, 3, 4]
        size idx `shouldBe` 4
