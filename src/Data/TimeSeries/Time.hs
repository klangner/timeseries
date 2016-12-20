-- | Helper functions operating on date and Time
module Data.TimeSeries.Time
    ( TimeResolution
    , days
    , months
    , nextTime
    , seconds
    , years
    ) where

import Data.Time( UTCTime(..)
                , NominalDiffTime
                , addDays
                , addGregorianMonthsClip
                , addUTCTime)


data TimeResolution = Years Integer
                    | Months Integer
                    | Days Integer
                    | Seconds NominalDiffTime


-- | Create Time Resolution from days
days :: Integer -> TimeResolution
days = Days


-- | Create Time Resolution from months
months :: Integer -> TimeResolution
months = Months


-- | Create Time Resolution from seconds
seconds :: Int -> TimeResolution
seconds n = Seconds (fromIntegral n)


-- | Create Time Resolution from years
years :: Integer -> TimeResolution
years = Years


-- | Return Time Series mean
nextTime :: TimeResolution -> UTCTime -> UTCTime
nextTime (Years n) (UTCTime d s) = UTCTime (addGregorianMonthsClip (12 * n) d) s
nextTime (Months n) (UTCTime d s) = UTCTime (addGregorianMonthsClip n d) s
nextTime (Days n) (UTCTime d s) = UTCTime (addDays n d) s
nextTime (Seconds n) utc = addUTCTime n utc
