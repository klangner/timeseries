module Data.TimeSeries.Index (
    DateTimeIndex,
    fromTimestamps,
    size
) where

import Prelude hiding (max, min, zip)
import qualified Data.Vector as V
import           Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)


data DateTimeIndex = MkIndex (V.Vector UTCTime)

-- | Create index from list of timestamps in seconds
fromTimestamps :: [Int] -> DateTimeIndex
fromTimestamps xs = MkIndex (V.fromList (map f xs))
    where
        f = posixSecondsToUTCTime . fromIntegral


-- | Get size (length) of the index
size :: DateTimeIndex -> Int
size (MkIndex xs) = V.length xs
