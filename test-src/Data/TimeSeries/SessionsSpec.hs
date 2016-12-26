module Data.TimeSeries.SessionsSpec (spec) where

import Prelude
import Test.Hspec
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time (NominalDiffTime)
import qualified Data.TimeSeries.Sessions as S

import qualified Data.TimeSeries as TS


sessionFromSeconds :: NominalDiffTime -> NominalDiffTime -> S.Session
sessionFromSeconds n m = S.Session (posixSecondsToUTCTime n) (posixSecondsToUTCTime m)

spec :: Spec
spec =

  describe "Sessions" $ do

    it "Without breaks" $ do
        let idx = [1..6]
        let values = repeat True
        S.find (TS.seconds 1) (TS.tsSeries idx values) `shouldBe` [sessionFromSeconds 1 6]

    it "Single session" $ do
        let idx = [1..]
        let values = [False, True, True, True, True, True, False, False]
        S.find (TS.seconds 1) (TS.tsSeries idx values) `shouldBe` [sessionFromSeconds 2 7]

    it "2 sessions" $ do
        let idx = [1..]
        let values = [False, True, True, True, False, False, True, False]
        S.find (TS.seconds 1) (TS.tsSeries idx values) `shouldBe` [ sessionFromSeconds 2 5
                                                                  , sessionFromSeconds 7 8]
    it "Single session 2 events" $ do
        let idx = [1..]
        let values = [False, True, True, False, True, True, False, False]
        S.find (TS.seconds 2) (TS.tsSeries idx values) `shouldBe` [sessionFromSeconds 2 7]

