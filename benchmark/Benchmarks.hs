
import Criterion.Main
import Control.DeepSeq
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Text.Time (parseISODateTime)
import qualified Data.TimeSeries as TS


instance NFData a => NFData (TS.Series a) where
    rnf xs = rnf (TS.toList xs)


seriesSize :: Integer
seriesSize = 10^6

index :: Integer -> UTCTime
index n = posixSecondsToUTCTime (fromIntegral n)

lastIndex :: UTCTime
lastIndex = index (seriesSize - 1)

bigSeries :: TS.Series Double
bigSeries = TS.tsSeries [1..seriesSize] [1..]


smallSeries :: TS.Series Double
smallSeries = TS.tsSeries [1..10^4] [1..]

startTime :: UTCTime
startTime = posixSecondsToUTCTime 1


main :: IO ()
main = defaultMain
    [ bgroup "Basic operations"
        [ bench "size"  $ nf TS.size bigSeries
        , bench "valueAt"  $ nf (TS.valueAt lastIndex) bigSeries
        , bench "slice"  $ nf (TS.valueAt (index 1) . TS.slice (index 2) lastIndex) bigSeries
        , bench "max"  $ nf maximum bigSeries
        , bench "fmap"  $ nf (fmap (+ 2)) bigSeries
        ]
    , bgroup "Group operation"
        [ bench "rolling" $ nf (TS.rolling (TS.seconds 20) sum) smallSeries
        , bench "resampling" $ nf (TS.resample startTime (TS.seconds 20)) smallSeries
        ]
    , bgroup "IO"
        [ bench "load CSV" $ nfIO (TS.loadCSV TS.NoHeader parseISODateTime "test-100K.csv") ]
    ]