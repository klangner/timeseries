module Data.TimeSeries.TimeSpec (spec) where

import Data.Time hiding (months)
import Test.Hspec
import Test.QuickCheck

import Data.TimeSeries


spec :: Spec
spec = do

  describe "Time resolution" $ do

    it "Add seconds" $ do
        let utc = UTCTime (fromGregorian 2014 1 1) 0
        nextTime (seconds 20) utc `shouldBe` UTCTime (fromGregorian 2014 1 1) 20

    it "Add days" $ do
        let utc = UTCTime (fromGregorian 2014 1 1) 0
        nextTime (days 20) utc `shouldBe` UTCTime (fromGregorian 2014 1 21) 0

    it "Add months" $ do
        let utc = UTCTime (fromGregorian 2014 1 1) 0
        nextTime (months 20) utc `shouldBe` UTCTime (fromGregorian 2015 9 1) 0

    it "Add year" $ do
        let utc = UTCTime (fromGregorian 2014 1 1) 0
        nextTime (years 20) utc `shouldBe` UTCTime (fromGregorian 2034 1 1) 0

