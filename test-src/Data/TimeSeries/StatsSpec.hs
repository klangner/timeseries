module Data.TimeSeries.StatsSpec (spec) where

import Test.Hspec
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import qualified Data.TimeSeries as TS


spec :: Spec
spec = do

  describe "Base" $ do

    it "mean" $ do
        let idx = [1..]
        let values = [1.0, 2.0, 3.0] :: [Double]
        TS.mean (TS.tsSeries idx values) `shouldBe` 2.0
