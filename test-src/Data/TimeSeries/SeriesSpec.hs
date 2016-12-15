module Data.TimeSeries.SeriesSpec (spec) where

import Test.Hspec
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import qualified Data.TimeSeries as TS


spec :: Spec
spec = do

  describe "Basic operations" $ do

    it "return series size" $ do
        let idx = [1, 2, 3]
        let values = [1.0, 2.0, 3.0]
        TS.size (TS.tsSeries idx values) `shouldBe` 3

    it "toList" $ do
        let idx = [1, 2, 3]
        let values = [1.0, 2.0, 3.0]
        let xs = TS.toList (TS.tsSeries idx values)
        map (\(x, y) -> (utcTimeToPOSIXSeconds x, y)) xs `shouldBe` [(1, 1.0), (2, 2.0), (3, 3.0)]

    it "return data point value at given index" $ do
        let idx = [10, 20, 30]
        let values = [10.0, 12.0, 32.4]
        let pos = posixSecondsToUTCTime $ fromIntegral 20
        TS.valueAt (TS.tsSeries idx values) pos `shouldBe` Just 12.0

    it "return Nothing value if wrong index" $ do
        let idx = [10, 20, 30]
        let values = [10.0, 12.0, 32.4]
        let pos = posixSecondsToUTCTime $ fromIntegral 1234
        TS.valueAt (TS.tsSeries idx values) pos `shouldBe` Nothing

    it "return subset" $ do
        let xs = TS.tsSeries [1..5] [10.0, 1.2, 32.4, 0.65, 11.0]
        let start = posixSecondsToUTCTime $ fromIntegral 2
        let end = posixSecondsToUTCTime $ fromIntegral 3
        TS.size (TS.slice xs start end) `shouldBe` 2

    it "return empty subset" $ do
        let xs = TS.tsSeries [1..5] [10.0, 1.2, 32.4, 0.65, 11.0]
        let start = posixSecondsToUTCTime $ fromIntegral 12
        let end = posixSecondsToUTCTime $ fromIntegral 34
        TS.slice xs start end `shouldBe` TS.emptySeries

    it "map over series" $ do
        let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
        let ys = fmap (+ 2) xs
        (TS.values ys)`shouldBe` [12.0, 3.2, 34.4, 2.65, 13.0]

    it "maximum value" $ do
        let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
        maximum xs `shouldBe` 32.4

    it "minimum value" $ do
        let xs = TS.tsSeries [1..5] [10.0, 1.2, 32.4, 0.65, 11.0]
        minimum xs `shouldBe` 0.65

