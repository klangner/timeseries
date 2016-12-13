module Data.TimeSeries.CSVSpec (spec) where

import Test.Hspec
import Data.ByteString.Time (parseISODateTime)

import qualified Data.TimeSeries as TS
import qualified Data.TimeSeries.CSVReader as CSV


spec :: Spec
spec = do

  describe "Reader" $ do

    it "co2 test data" $ do
        xs <- CSV.loadCSV parseISODateTime "testdata/co2.csv"
        TS.size xs `shouldBe` 192
