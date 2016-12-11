
import Criterion.Main
import qualified Data.TimeSeries as TS


bigSeries :: TS.Series
bigSeries = TS.series [1..10^n] [1..10^n]
    where n = 6


main :: IO ()
main = defaultMain
    [ bgroup "Big series"
        [ bench "size"  $ nf TS.size bigSeries
        , bench "valueAt"  $ nf (\xs -> TS.valueAt xs (10^6-1)) bigSeries
        , bench "slice"  $ nf (\xs -> TS.valueAt (TS.slice xs 2 (10^6-2)) 1) bigSeries
        ]
    ]