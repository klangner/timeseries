module Data.TimeSeries.SeriesSpec (spec) where

import qualified Data.TimeSeries as TS
import Test.Hspec


spec :: Spec
spec = do

  describe "Basic operations" $
    it "return series size" $ do
        let idx = [1, 2, 3]
        let values = [1.0, 2.0, 3.0]
        TS.size (TS.series idx values) `shouldBe` 3
