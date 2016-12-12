
import Criterion.Main
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.TimeSeries as TS


seriesSize :: Integer
seriesSize = 10^6

index :: Integer -> UTCTime
index n = posixSecondsToUTCTime (fromIntegral n)

lastIndex :: UTCTime
lastIndex = index (seriesSize - 1)

bigSeries :: TS.Series
bigSeries = TS.tsSeries [1..seriesSize] [1..]


main :: IO ()
main = defaultMain
    [ bgroup "Big series"
        [ bench "size"  $ nf TS.size bigSeries
        , bench "valueAt"  $ nf (\xs -> TS.valueAt xs lastIndex) bigSeries
        , bench "slice"  $ nf (\xs -> TS.valueAt (TS.slice xs (index 2) lastIndex) (index 1)) bigSeries
        ]
    ]