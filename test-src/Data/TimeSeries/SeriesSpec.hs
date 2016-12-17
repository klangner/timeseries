module Data.TimeSeries.SeriesSpec (spec) where

import Test.Hspec
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Statistics.Sample as S

import qualified Data.TimeSeries as TS


sampleSeries :: TS.Series Double
sampleSeries = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.6, 11.0]


spec :: Spec
spec = do

  describe "Converting Series" $ do

    it "toList" $ do
        let xs = TS.toList sampleSeries
        map (\(x, y) -> (utcTimeToPOSIXSeconds x, y)) xs `shouldBe` [(1, 10.0), (2, 1.2), (3, 32.4), (4, 0.6), (5, 11.0)]


  describe "Selecting sub Series" $ do

    it "return data point value at given index" $ do
        let pos = posixSecondsToUTCTime $ fromIntegral 2
        TS.valueAt pos sampleSeries `shouldBe` Just 1.2

    it "return Nothing value if wrong index" $ do
        let idx = [10, 20, 30]
        let values = [10.0, 12.0, 32.4]
        let pos = posixSecondsToUTCTime $ fromIntegral 1234
        TS.valueAt pos (TS.tsSeries idx values) `shouldBe` Nothing

    it "return subset" $ do
        let xs = TS.tsSeries [1..5] [10.0, 1.2, 32.4, 0.65, 11.0]
        let start = utcFromSeconds 2
        let end = utcFromSeconds 3
        TS.size (TS.slice start end xs) `shouldBe` 2

    it "return empty subset" $ do
        let xs = TS.tsSeries [1..5] [10.0, 1.2, 32.4, 0.65, 11.0]
        let start = utcFromSeconds 12
        let end = utcFromSeconds 34
        TS.slice start end xs `shouldBe` TS.emptySeries


  describe "Basic operations" $ do

    it "return series size" $ do
        let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
        TS.size xs `shouldBe` 5

    it "map over series" $ do
        let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
        let ys = fmap (+ 2) xs
        TS.values ys `shouldBe` [12.0, 3.2, 34.4, 2.65, 13.0]

    it "fold over series" $ do
        foldr (\v acc -> v + acc) 0.0 sampleSeries `shouldBe` 55.2

    it "maximum value" $ do
        let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
        maximum xs `shouldBe` 32.4

    it "minimum value" $ do
        let xs = TS.tsSeries [1..5] [10.0, 1.2, 32.4, 0.65, 11.0]
        minimum xs `shouldBe` 0.65


  describe "Group operations" $ do

    it "rolling window" $ do
        let xs = TS.tsSeries [1..] [1.0, 2.0, 3.0, 4.0, 5.0]
        TS.rolling 2 sum xs `shouldBe` TS.tsSeries [2..] [3.0, 5.0, 7.0, 9.0]

--     it "smoothing" $ do
--         let xs = TS.tsSeries [1..] [1.0, 2.0, 3.0, 4.0, 5.0]
--         TS.rolling 3 S.mean xs `shouldBe` TS.tsSeries [3..] [2.0, 3.0, 4.0]



utcFromSeconds :: Integer -> UTCTime
utcFromSeconds = posixSecondsToUTCTime . fromIntegral