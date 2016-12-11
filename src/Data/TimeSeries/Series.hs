{-|
Module:      Data.TimeSeries.Series
Copyright:   (c) 2016 Krzysztof Langner
License:     BSD3
Stability:   experimental
Portability: portable
Definition and basic operation on Series.
-}

module Data.TimeSeries.Series
    ( DataPoints
    , Index
    , Series
    , valueAt
    , series
    , size
    ) where

import Prelude
import qualified Data.List as L


type Timestamp = Integer
type Value = Double
type Index = [Timestamp]
type DataPoints = [Value]

-- Memory size: ???
data Series = Series Index DataPoints


-- | Create new series
-- >series [1, 2, 3] [41.3, 52.22, 3.0] == Series [1, 2, 3] [41.3, 52.22, 3.0]
series :: Index -> DataPoints -> Series
series idx vs = Series idx vs


-- | Get series size.
-- Complexity O(n)
--
-- >size (Series [1, 2, 3] [41.3, 52.22, 3.0]) == 3
--
size :: Series -> Int
size (Series idx _) = length idx


-- | Return data point value at given index
-- Complexity O(n)
--
-- >valueAt (Series [1, 2, 3] [41.3, 52.22, 3.0]) 2 == Just 52.22
-- >valueAt (Series [1, 2, 3] [41.3, 52.22, 3.0]) 5 == Nothing
--
valueAt :: Series -> Timestamp -> Maybe Value
valueAt (Series idx xs) ts = fmap (\i -> xs !! i) ix
    where ix = L.elemIndex ts idx
