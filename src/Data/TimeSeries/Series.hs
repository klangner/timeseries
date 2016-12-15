{-# OPTIONS_GHC -funbox-strict-fields  #-}
{-|
Module:      Data.TimeSeries.Series
Copyright:   (c) 2016 Krzysztof Langner
License:     BSD3
Stability:   experimental
Portability: portable
Definition and basic operation on Series.
-}

module Data.TimeSeries.Series
    ( DataPoint
    , Series
    , emptySeries
    , series
    , size
    , slice
    , toList
    , tsSeries
    , valueAt
    , values
    ) where

import Prelude hiding (max, min)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)


-- | Data points is single index value od time series
data DataPoint a = DP !UTCTime a
                 deriving (Show, Eq)

instance Functor DataPoint where
    fmap f (DP x y) = DP x (f y)

instance Foldable DataPoint where
    foldMap f (DP _ y) = f y

-- | Data structure for holding Series.
-- Implementation should be hidden so it can be changed in the future
data Series a = Series [DataPoint a]
    deriving (Show, Eq)

instance Functor Series where
    fmap f (Series xs) = Series (map (fmap f) xs)

instance Foldable Series where
    foldMap f (Series xs) = foldMap (foldMap f) xs


-- | Create empty series
emptySeries :: Series a
emptySeries = Series []


-- | Create series
series :: [(UTCTime, a)] -> Series a
series xs = Series $ map (\(x, y) -> DP x y) xs


-- | Create time series from timestamps and values
--
-- >seriesFromSeconds [1, 2, 3] [41.3, 52.22, 3.0] == Series [DP 1970-01-01 00:00:01 UTC 2.3,DP 1970-01-01 00:00:02 UTC 4.5]
--
tsSeries :: [Integer] -> [a] -> Series a
tsSeries ts vs = Series (zipWith DP idx vs)
    where idx = map (posixSecondsToUTCTime . fromIntegral) ts


-- | Convert Time Series to list
toList :: Series a -> [(UTCTime, a)]
toList (Series xs) = map (\(DP x y) -> (x, y)) xs


-- | Get only series values
values :: Series a -> [a]
values ts = map (\(_, y) -> y) (toList ts)


-- | Get series size.
-- Complexity O(n)
--
-- >size (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) == 3
--
size :: Series a -> Int
size (Series xs) = length xs


-- | Fold the values
vfold :: (b -> a -> b) -> b -> Series a -> b
vfold f r (Series xs) = foldl (\d (DP _ y) -> f d y) r xs


-- | Return data point value at given index
-- Complexity O(n)
--
-- >valueAt (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 2 == Just 52.22
-- >valueAt (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 5 == Nothing
--
valueAt :: Series a -> UTCTime -> Maybe a
valueAt (Series xs) ts = safeHead [y | DP x y <- xs, x == ts]
    where safeHead [] = Nothing
          safeHead (i:_) = Just i


-- | Return series subset
-- Complexity O(n)
--
-- >slice (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 2 3 == Series [DP 2 52.22, DP 3 3.0]
-- >slice (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 5 23 == Series []
--
slice :: Series a -> UTCTime -> UTCTime -> Series a
slice (Series xs) start end = Series [DP x y | DP x y <- xs, x >= start && x <= end]


