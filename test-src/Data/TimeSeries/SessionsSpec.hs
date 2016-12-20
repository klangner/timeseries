module Data.TimeSeries.SessionsSpec (spec) where

import Test.Hspec
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.TimeSeries.Sessions as S

import qualified Data.TimeSeries as TS


spec :: Spec
spec =

  describe "Sessions" $

    it "Without breaks" $ do
        let idx = [1..]
        let values = [1.0..6.0] :: [Double]
        S.find (TS.tsSeries idx values) `shouldBe` []
