module Data.TimeSeries (
    -- * Series datatype
    TimeSeries,
    ts,
    index,
    values,
    size
    ) where

import qualified Data.Vector as V
import qualified Data.TimeSeries.Date as DTI


data TimeSeries = MkSeries DTI.DateTimeIndex V.Vector


-- | Create Time Series
ts :: DTI.DateTimeIndex -> V.Vector -> TimeSeries
ts idx vs = MkSeries idx vs


-- | Gen series index
index :: TimeSeries -> DTI.DateTimeIndex
index (MkSeries idx _) = idx

-- | Get Series values
values :: TimeSeries -> V.Vector
values (MkSeries _ vs) = vs

-- | Get series size
size :: TimeSeries -> Int
size (Mk idx vs) = DTI.size idx
