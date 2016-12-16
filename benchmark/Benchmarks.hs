
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


main :: IO ()
main = defaultMain
    [ bgroup "Basic operations"
        [ bench "size"  $ nf TS.size bigSeries
        , bench "valueAt"  $ nf (\xs -> TS.valueAt xs lastIndex) bigSeries
        , bench "slice"  $ nf (\xs -> TS.valueAt (TS.slice xs (index 2) lastIndex) (index 1)) bigSeries
        , bench "max"  $ nf maximum bigSeries
        , bench "fmap"  $ nf (fmap (+ 2)) bigSeries
        ]
    , bgroup "IO"
        [ bench "load CSV" $ nfIO (TS.loadCSV TS.NoHeader parseISODateTime "testdata/test-100K.csv") ]
    ]