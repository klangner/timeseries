import Data.Time
import Data.Text.Time (parseISODateTime)

import qualified Data.TimeSeries as TS
import qualified Data.TimeSeries.Sessions as S


main = do
    ts <- TS.loadCSV TS.HasHeader parseISODateTime "testdata/rain.csv"
    let utc = UTCTime (fromGregorian 2013 7 1) 0
    let xs = S.find (TS.seconds (6*3600)) (fmap (\x -> x > 0.1) ts)
    let xs' = filter (\(S.Session x y) -> x < utc) xs
    mapM (putStrLn . show) xs'
