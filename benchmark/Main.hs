
import Criterion.Main
import qualified Data.TimeSeries as TS


bigSeries :: TS.Series
bigSeries = TS.series [1..10^n] [1..10^n]
    where n = 6


main :: IO ()
main = defaultMain
    [ bgroup "Big series (10^6)"
        [ bench "size"  $ nf TS.size bigSeries
        ]
    ]