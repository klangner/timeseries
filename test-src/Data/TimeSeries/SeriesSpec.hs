module Data.TimeSeries.SeriesSpec (spec) where

import Prelude
import Test.Hspec
import Test.QuickCheck
import Control.Arrow (first)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Statistics.Sample as S
import qualified Data.Vector as V

import qualified Data.TimeSeries as TS
import Data.TimeSeries.Time (seconds)


sampleSeries :: TS.TimeSeries Double
sampleSeries = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.6, 11.0]


spec :: Spec
spec = do

  describe "Properties" $ do

    it "toList" $ property $
        \ts -> (TS.series . TS.toList) ts == (ts :: TS.TimeSeries Double)

    it "fold over series" $ property $
        \ts -> (foldr (+) 0.0 ts) - ((sum . (map snd) . TS.toList) (ts :: TS.TimeSeries Double)) < 10**(-6)

    it "resampling the same" $ property $
        \ts -> TS.resample (posixSecondsToUTCTime 1) (seconds 1) ts == (ts :: TS.TimeSeries Double)

    it "groupBy the same" $ property $
        \ts -> TS.groupBy (seconds 1) sum ts == (ts :: TS.TimeSeries Double)


  describe "Selecting sub Series" $ do

    it "return data point value at given index" $ do
        let pos = posixSecondsToUTCTime 2
        TS.valueAt pos sampleSeries `shouldBe` Just 1.2

    it "return Nothing value if wrong index" $ do
        let idx = [10, 20, 30]
        let values = [10.0, 12.0, 32.4]
        let pos = posixSecondsToUTCTime 1234
        TS.valueAt pos (TS.tsSeries idx values) `shouldBe` Nothing

    it "return subset" $ do
        let xs = TS.tsSeries [1..5] [10.0, 1.2, 32.4, 0.65, 11.0]
        let start = utcFromSeconds 2
        let end = utcFromSeconds 3
        TS.size (TS.slice start end xs) `shouldBe` 2

    it "return range" $ do
        let xs = TS.tsSeries [0, 5..] [1.0, 1.2, 32.4, 0.65, 11.0]
        let start = utcFromSeconds 2
        let end = utcFromSeconds 130
        TS.size (TS.slice start end xs) `shouldBe` 4

    it "return empty subset" $ do
        let xs = TS.tsSeries [1..5] [10.0, 1.2, 32.4, 0.65, 11.0]
        let start = utcFromSeconds 12
        let end = utcFromSeconds 34
        TS.slice start end xs `shouldBe` TS.emptySeries


  describe "Basic operations" $ do

    it "return series size" $ do
        let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
        TS.size xs `shouldBe` 5

    it "first element" $ do
        let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
        fmap TS.dpValue (TS.firstElem xs) `shouldBe` Just 10.0

    it "last element" $ do
        let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
        fmap TS.dpValue (TS.lastElem xs) `shouldBe` Just 11.0

    it "element by index" $ do
        let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
        fmap TS.dpValue (TS.elemAt 2 xs) `shouldBe` Just 32.4

    it "map over series" $ do
        let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
        let ys = fmap (+ 2) xs
        TS.values ys `shouldBe` [12.0, 3.2, 34.4, 2.65, 13.0]

    it "maximum value" $ do
        let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
        maximum xs `shouldBe` 32.4

    it "minimum value" $ do
        let xs = TS.tsSeries [1..5] [10.0, 1.2, 32.4, 0.65, 11.0]
        minimum xs `shouldBe` 0.65


  describe "Rolling" $ do

    it "sum" $ do
        let xs = TS.tsSeries [1..] [1.0, 2.0, 3.0, 4.0, 5.0]
        TS.rolling (TS.seconds 2) sum xs `shouldBe` TS.tsSeries [2..] [3.0, 5.0, 7.0, 9.0]


  describe "Smoothing" $ do

    it "mean" $ do
        let xs = TS.tsSeries [1..] [1.0, 2.0, 3.0, 4.0, 5.0]
        TS.rolling (TS.seconds 3) (S.mean . V.fromList) xs `shouldBe` TS.tsSeries [3..] [2.0, 3.0, 4.0]


  describe "Resampling" $ do

    it "missing values" $ do
        let xs = TS.tsSeries [1, 2, 3, 5] [1.0, 2.0, 3.0, 5.0]
        let startTime = posixSecondsToUTCTime 1
        TS.resample startTime (seconds 1) xs `shouldBe` TS.tsSeries [1, 2, 3, 4, 5] [1.0, 2.0, 3.0, 4.0, 5.0]

    it "missing lots of values" $ do
        let xs = TS.tsSeries [1, 5] [1.0, 5.0]
        let startTime = posixSecondsToUTCTime 1
        TS.resample startTime (seconds 1) xs `shouldBe` TS.tsSeries [1, 2, 3, 4, 5] [1.0, 2.0, 3.0, 4.0, 5.0]

    it "middle values" $ do
        let xs = TS.tsSeries [1, 3, 5] [1.0, 3.0, 5.0]
        let startTime = posixSecondsToUTCTime 0
        TS.resample startTime (seconds 2) xs `shouldBe` TS.tsSeries [0, 2, 4] [1.0, 2.0, 4.0]


  describe "Group by" $ do

    it "sum" $ do
        let xs = TS.tsSeries [1..] [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
        TS.groupBy (TS.seconds 2) sum xs `shouldBe` TS.tsSeries [1, 3, 5] [3.0, 7.0, 11.0]


  describe "zip" $ do

    it "the same index" $ do
        let xs = TS.tsSeries [1..] [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
        let ys = TS.tsSeries [1..] [2.0, 3.0, 4.0, 5.0, 6.0, 7.0]
        fmap (\(x,y) -> x+y) (TS.zip xs ys) `shouldBe` TS.tsSeries [1..] [3.0, 5.0, 7.0, 9.0, 11.0, 13.0]



-- Convert to UTC Time from timestamp
utcFromSeconds :: Integer -> UTCTime
utcFromSeconds = posixSecondsToUTCTime . fromIntegral


-- Generate Time Series
instance (Arbitrary a) => Arbitrary (TS.TimeSeries a) where
    arbitrary = do
        vs <- arbitrary
        return $ TS.tsSeries [1..] vs
