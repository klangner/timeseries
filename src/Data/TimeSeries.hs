{-# OPTIONS_GHC -funbox-strict-fields  #-}
module Data.TimeSeries (
    -- * Series datatype
    TimeSeries,
    ts,
    emptySeries,
    index,
    values,
    length
    ) where

import Prelude hiding (length)
import qualified Data.Vector as V
import           Data.Time (UTCTime)


data TimeSeries = MkSeries (V.Vector UTCTime) (V.Vector Float)


-- | Create Time Series
ts :: V.Vector UTCTime -> V.Vector Float -> TimeSeries
ts idx vs = MkSeries idx vs

-- | Create empty Time Series
emptySeries :: TimeSeries
emptySeries = MkSeries V.empty V.empty

-- | Gen series index
index :: TimeSeries -> V.Vector UTCTime
index (MkSeries idx _) = idx

-- | Get Series values
values :: TimeSeries -> (V.Vector Float)
values (MkSeries _ vs) = vs

-- | Get series length
length :: TimeSeries -> Int
length (MkSeries idx _) = V.length idx
