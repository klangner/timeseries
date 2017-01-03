{-# OPTIONS_GHC -funbox-strict-fields  #-}
{-|
Module:      Data.TimeSeries.Series
Copyright:   (c) 2016 Krzysztof Langner
License:     BSD3
Stability:   experimental
Portability: portable
Definition and basic operation on Series.
-}

module Data.TimeSeries.Series (
    -- * Series datatype
      DataPoint
    , Series
    , dpIndex
    , dpValue
    -- * Create series
    , emptySeries
    , tsSeries
    , series
    -- * Conversion between types
    , toList
    , values
    -- * Selecting data from series
    , slice
    , valueAt
    -- * Transformations
    , groupBy
    , rolling
    , resample
    , size
    , zip
    ) where

import Prelude hiding (max, min, zip)
import Data.Time ( UTCTime
                 , diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Data.TimeSeries.Time (TimeResolution, nextTime)


-- | Data points is a time indexed value
data DataPoint a = DP { dpIndex :: !UTCTime     -- ^ Get data point index.
                      , dpValue :: a            -- ^ Get data point value.
                      }
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
    length = size


-- | Create empty series.
emptySeries :: Series a
emptySeries = Series []


-- | Create series from UTCTime and value.
series :: [(UTCTime, a)] -> Series a
series xs = Series $ map (uncurry DP) xs


-- | Create time series from timestamps and values
--
-- >seriesFromSeconds [1, 2, 3] [41.3, 52.22, 3.0] == Series [DP 1970-01-01 00:00:01 UTC 2.3,DP 1970-01-01 00:00:02 UTC 4.5]
--
tsSeries :: [Integer]       -- ^ List of index value given as number of seconds
         -> [a]             -- ^ List of value
         -> Series a        -- ^ Created Series
tsSeries ts vs = Series (zipWith DP idx vs)
    where idx = map (posixSecondsToUTCTime . fromIntegral) ts


-- | Convert Time Series to the list.
toList :: Series a -> [(UTCTime, a)]
toList (Series xs) = map (\(DP x y) -> (x, y)) xs


-- | Get series values as list.
values :: Series a -> [a]
values ts = map snd (toList ts)


-- | Get series size.
-- Complexity O(n)
--
-- >size (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) == 3
--
size :: Series a -> Int
size (Series xs) = length xs


-- | Return data point value at given index
-- Complexity O(n)
--
-- >valueAt (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 2 == Just 52.22
-- >valueAt (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 5 == Nothing
--
valueAt :: UTCTime      -- ^ Index position
        -> Series a     -- ^ Input Series
        -> Maybe a      -- ^ Value at given index
valueAt ts (Series xs) = safeHead [y | DP x y <- xs, x == ts]
    where safeHead [] = Nothing
          safeHead (i:_) = Just i


-- | Return series subset.
-- Complexity O(n)
--
-- >slice (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 2 3 == Series [DP 2 52.22, DP 3 3.0]
-- >slice (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 5 23 == Series []
--
slice :: UTCTime        -- ^ Start time (inclusive)
      -> UTCTime        -- ^ End time (inclusive)
      -> Series a       -- ^ Input series
      -> Series a       -- ^ Sliced Series
slice start end (Series xs) = Series [DP x y | DP x y <- xs, x >= start && x <= end]


-- | Apply rolling window to create a new Series.
-- Rolling window is also called Sliding Window.
rolling :: TimeResolution   -- ^ Window size
        -> ([a] -> b)       -- ^ Function applied to each window
        -> Series a         -- ^ Input Series
        -> Series b         -- ^ Converted Series
rolling dt f (Series xs) = Series $ map (\(i, vs) -> DP i (f vs)) (windows dt xs)

-- Create rolling windows based on given delta time.
windows :: TimeResolution -> [DataPoint a] -> [(UTCTime, [a])]
windows _ [] = []
windows dt xs = g ys : if length xs > length ys then windows dt (tail xs) else []
    where
        -- Take data points from window based on time difference [DataPoint]
        ys = takeWhile (isInTimeRange dt (head xs)) xs
        -- Convert [DataPoint a] -> (UTCTime, [a])
        g vs = (dpIndex (last vs), values (Series vs))

-- Check if two DataPoints are closer then given time difference
isInTimeRange :: TimeResolution -> DataPoint a -> DataPoint a -> Bool
isInTimeRange dt (DP i _) (DP j _) = j < nextTime dt i


-- | Resample Series.
-- Resample takes weighted mean value between 2 data points.
-- Where weight is based on distance to resampled point.
-- This helps to provide approximate value in the case of lots of missing values
resample :: Fractional a
         => UTCTime             -- ^ Starting time
         -> TimeResolution      -- ^ Resampling resolution
         -> Series a            -- ^ Input series
         -> Series a            -- ^ Resampled series
resample _ _ (Series []) = emptySeries
resample utc res (Series xs) = Series (resample' utc res (head xs) xs)

-- | Resample based on list
resample' :: Fractional a => UTCTime -> TimeResolution -> DataPoint a -> [DataPoint a] -> [DataPoint a]
resample' _ _ _ [] = []
resample' utc res y (x:xs)
    | utc < dpIndex x   = DP utc mu : resample' utc2 res y (x:xs)
    | utc == dpIndex x  = DP utc (dpValue x) : resample' utc2 res x xs
    | otherwise         = resample' utc res x xs
    where
        utc2 = nextTime res utc
        mu = (ty/(tx+ty)) * dpValue x + ((tx/(tx+ty)) * dpValue y)
        tx = abs $ realToFrac (diffUTCTime utc (dpIndex x))
        ty = abs $ realToFrac (diffUTCTime utc (dpIndex y))


-- | Group data by a given time frame
-- This function expect that the time series has enough data points to group values.
groupBy :: TimeResolution   -- ^ Window size
        -> ([a] -> b)       -- ^ Function applied to group values
        -> Series a         -- ^ Input Series
        -> Series b         -- ^ Converted Series
groupBy _ _ (Series []) = emptySeries
groupBy res f (Series xs) = Series (map (\(i, vs) -> DP i (f (g vs))) (groupBy' utc res xs))
    where
        g :: [DataPoint a] -> [a]
        g  = map dpValue
        utc = dpIndex (head xs)


groupBy' :: UTCTime -> TimeResolution -> [DataPoint a] -> [(UTCTime, [DataPoint a])]
groupBy' _ _ [] = []
groupBy' utc res xs = (utc, ys) : groupBy' utc2 res zs
    where
        utc2 = nextTime res utc
        (ys, zs) = break (\(DP x _) -> x >= utc2) xs


-- | Zip 2 series into one. Only keep elements with the same index value.
-- This function also assumes that data points are sorted by index value
zip :: Series a -> Series b -> Series (a, b)
zip (Series xs) (Series ys) = Series $ zip' xs ys


zip' :: [DataPoint a] -> [DataPoint b] -> [DataPoint (a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys)
    | dpIndex x > dpIndex y = zip' xs (y:ys)
    | dpIndex x < dpIndex y = zip' (x:xs) ys
    | otherwise             = DP (dpIndex x) (dpValue x, dpValue y) : zip' xs ys


