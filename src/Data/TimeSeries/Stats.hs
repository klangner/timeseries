
module Data.TimeSeries.Stats
    ( mean
    ) where

import Prelude hiding (max, min)
import Data.TimeSeries.Series (Series)


-- | Return maximum value in the series
mean :: Fractional a => Series a -> a
mean xs = sum xs / fromIntegral (length xs)

