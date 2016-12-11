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
    , Timestamp
    , Value
    , max
    , min
    , series
    , size
    , slice
    , valueAt
    ) where

import Prelude hiding (max, min)
import Data.Word (Word64)


type Timestamp = Word64
type Value = Double
data DataPoint = DP {-# UNPACK #-} !Timestamp
                    {-# UNPACK #-} !Value
                 deriving (Show, Eq)

data Series = Series [DataPoint]
    deriving (Show, Eq)


-- | Create a new series
--
-- >series [1, 2, 3] [41.3, 52.22, 3.0] == Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]
--
series :: [Timestamp] -> [Value] -> Series
series idx vs = Series (zipWith DP idx vs)


-- | Get series size.
-- Complexity O(n)
--
-- >size (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) == 3
--
size :: Series -> Int
size (Series xs) = length xs


-- | Return data point value at given index
-- Complexity O(n)
--
-- >valueAt (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 2 == Just 52.22
-- >valueAt (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 5 == Nothing
--
valueAt :: Series -> Timestamp -> Maybe Value
valueAt (Series xs) ts = safeHead [y | DP x y <- xs, x == ts]
    where safeHead [] = Nothing
          safeHead (y:ys) = Just y


-- | Return series subset
-- Complexity O(n)
--
-- >slice (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 2 3 == Series [DP 2 52.22, DP 3 3.0]
-- >slice (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) 5 23 == Series []
--
slice :: Series -> Timestamp -> Timestamp -> Series
slice (Series xs) start end = Series [DP x y | DP x y <- xs, x >= start && x <= end]


-- | Return maximum value in the series
-- Complexity O(n)
--
-- >max (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) == 52.22
-- >max (Series []) == 0
--
max :: Series -> Value
max (Series xs) = foldl (\d (DP _ y) -> max' d y) 0.0 xs
    where max' a b = if a > b then a else b

-- | Return maximum value in the series
-- Complexity O(n)
--
-- >min (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) == 3.0
-- >min (Series []) == 0
--
min :: Series -> Value
min (Series xs) = foldl (\d (DP _ y) -> min' d y) init xs
    where min' a b = if a < b then a else b
          init = fromIntegral (maxBound :: Int)
