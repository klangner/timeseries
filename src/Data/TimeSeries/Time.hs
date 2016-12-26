-- | Helper functions operating on date and Time
module Data.TimeSeries.Time
    ( TimeResolution
    , days
    , months
    , nextTime
    , seconds
    , years
    ) where

import Prelude (Integer, Int, fromIntegral, (*))
import Data.Time( UTCTime(..)
                , NominalDiffTime
                , addDays
                , addGregorianMonthsClip
                , addUTCTime)


-- | TimeResolution is for holding time periods which can't easy be translated to number of seconds
--   For example day doesn't always have 86400 and month have different number of days.
--   So if we want to define 1 month period we can't use DiffTime for it.
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
