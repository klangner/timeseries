module Data.TimeSeries.IO.CSVSpec (spec) where

import Test.Hspec
import Data.Text.Time (parseISODateTime)

import qualified Data.TimeSeries as TS
import qualified Data.TimeSeries.IO.CSVReader as CSV


spec :: Spec
spec = do

  describe "Reader" $ do

    it "co2 test data" $ do
        xs <- CSV.loadCSV HasHeader parseISODateTime "testdata/co2.csv"
        TS.size xs `shouldBe` 192
