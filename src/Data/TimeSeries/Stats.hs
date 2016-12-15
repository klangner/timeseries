
module Data.TimeSeries.Stats
    ( max
    , min
    ) where

import Prelude hiding (max, min)
import Data.TimeSeries.Series (Series, Value, vfold)


-- | Return maximum value in the series
-- Complexity O(n)
--
-- >max (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) == 52.22
-- >max (Series []) == 0
--
max :: Series -> Value
max xs = vfold max' 0.0 xs
    where max' a b = if a > b then a else b

-- | Return maximum value in the series
-- Complexity O(n)
--
-- >min (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) == 3.0
-- >min (Series []) == 0
--
min :: Series -> Value
min xs = vfold min' maxValue xs
    where min' a b = if a < b then a else b
          maxValue = fromIntegral (maxBound :: Int)
