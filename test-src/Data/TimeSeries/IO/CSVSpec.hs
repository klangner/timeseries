module Data.TimeSeries.IO.CSVSpec (spec) where

import Prelude
import Test.Hspec
import Data.Text.Time (parseISODateTime)

import qualified Data.TimeSeries as TS
import qualified Data.TimeSeries.IO.CSVReader as CSV


spec :: Spec
spec =

  describe "Reader" $

    it "co2 test data" $ do
        xs <- CSV.loadCSV CSV.HasHeader parseISODateTime "testdata/co2.csv"
        TS.size xs `shouldBe` 192
