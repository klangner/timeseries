module Data.TimeSeries.Index (
    DateTimeIndex,
    fromTimestamps,
    size
) where

import Data.Int
import qualified Data.Vector as V


type Timestamp = Int64

data DateTimeIndex = MkIndex (V.Vector Timestamp)

-- | Create index from list of timestamps
fromTimestamps :: [Timestamp] -> DateTimeIndex
fromTimestamps xs = MkIndex (V.fromList xs)


-- | Get size (length) of the index
size :: DateTimeIndex -> Int
size (MkIndex xs) = V.length xs
