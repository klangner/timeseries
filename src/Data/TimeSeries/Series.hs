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
    , dpIndex
    , dpValue
    , emptySeries
    , rolling
    , series
    , size
    , slice
    , toList
    , toVector
    , tsSeries
    , valueAt
    , values
    ) where

import Prelude hiding (max, min)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)


-- | Data points is single index value od time series
data DataPoint a = DP { dpIndex :: !UTCTime
                      , dpValue :: a }
                 deriving (Show, Eq)

instance Functor DataPoint where
    fmap f (DP x y) = DP x (f y)

instance Foldable DataPoint where
    foldMap f (DP _ y) = f y

-- | Data structure for holding Series.
-- Implementation should be hidden so it can be changed in the future
data Series a = Series (V.Vector (DataPoint a))
    deriving (Show, Eq)

instance Functor Series where
    fmap f (Series xs) = Series (V.map (fmap f) xs)

instance Foldable Series where
    foldMap f (Series xs) = foldMap (foldMap f) xs
    length = size


-- | Create empty series
emptySeries :: Series a
emptySeries = Series V.empty


-- | Create series
series :: V.Vector (UTCTime, a) -> Series a
series xs = Series $ V.map (\(x, y) -> DP x y) xs


-- | Create time series from timestamps and values
--
-- >seriesFromSeconds [1, 2, 3] [41.3, 52.22, 3.0] == Series [DP 1970-01-01 00:00:01 UTC 2.3,DP 1970-01-01 00:00:02 UTC 4.5]
--
tsSeries :: [Integer]       -- ^ List of index value given as number of seconds
         -> [a]             -- ^ List of value
         -> Series a        -- ^ Created Series
tsSeries ts vs = Series (V.fromList (zipWith DP idx vs))
    where idx = map (posixSecondsToUTCTime . fromIntegral) ts


-- | Convert Time Series to list
toList :: Series a -> [(UTCTime, a)]
toList (Series xs) = V.toList $ V.map (\(DP x y) -> (x, y)) xs


-- | Convert Time Series to vector
toVector :: Series a -> V.Vector (UTCTime, a)
toVector (Series xs) = V.map (\(DP x y) -> (x, y)) xs


-- | Get only series values
values :: Series a -> V.Vector a
values (Series xs) = V.map (\(DP _ y) -> y) xs


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
valueAt t dt = fmap dpValue (t !? dt)


-- | Safe indexing
-- Complexity O(n)
(!?) :: UTCTime                 -- ^ Index position
     -> Series a                -- ^ Input Series
     -> Maybe (DataPoint a)     -- ^ Data point  at given index
(!?) t (Series xs) = fmap (\v -> V.unsafeIndex xs v) (findVectorPos t xs)


-- Safe find vector position based on series index
findVectorPos :: UTCTime -> V.Vector (DataPoint a) -> Maybe Int
findVectorPos ts xs = V.findIndex (\(DP i _) -> i == ts) xs


-- | Return series subset
-- Complexity O(n)
--
-- >slice (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 2 3 == Series [DP 2 52.22, DP 3 3.0]
-- >slice (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 5 23 == Series []
--
slice :: UTCTime        -- ^ Start time (inclusive)
      -> UTCTime        -- ^ End time (inclusive)
      -> Series a       -- ^ Input series
      -> Series a       -- ^ Sliced Series
slice start end (Series xs) = Series (V.slice i j xs)
    where
        i = fromMaybe 0 (findVectorPos start xs)
        j = fromMaybe ((length xs) - 1) (findVectorPos end xs)


-- | Apply function to rolling window to create new Series
-- Rolling window is also called Sliding Window.
rolling ::  NominalDiffTime     -- ^ Window size
        -> (V.Vector a -> b)    -- ^ Function applied to each window
        -> Series a             -- ^ Input Series
        -> Series b             -- ^ Converted Series
rolling dt f (Series xs) = Series $ V.map (\(i, vs) -> DP i (f vs)) (windows dt xs)

-- Create rolling windows based on given delta time.
windows ::  NominalDiffTime -> V.Vector (DataPoint a) -> V.Vector (UTCTime, V.Vector a)
windows _  xs |  V.null xs = V.empty
windows dt xs = V.cons (g ys) (if V.length xs > V.length ys then windows dt (V.tail xs) else V.empty)
    where
        -- Take data points from window based on time difference [DataPoint]
        ys = V.takeWhile (isInTimeRange dt (V.head xs)) xs
        -- Convert [DataPoint a] -> (UTCTime, [a])
        g vs = (dpIndex (V.last vs), values (Series vs))

-- windows ::  NominalDiffTime -> V.Vector (DataPoint a) -> V.Vector (UTCTime, V.Vector a)
-- windows _ [] = []
-- windows dt xs = g ys : if V.length xs > V.length ys then windows dt (V.tail xs) else []
--     where
--         -- Take data points from window based on time difference [DataPoint]
--         ys = takeWhile (isInTimeRange dt (head xs)) xs
--         -- Convert [DataPoint a] -> (UTCTime, [a])
--         g vs = (dpIndex (last vs), values (Series vs))

-- Check if two DataPoints are closer then given time difference
isInTimeRange :: NominalDiffTime -> DataPoint a -> DataPoint a -> Bool
isInTimeRange dt (DP i _) (DP j _) = diffUTCTime j i < dt