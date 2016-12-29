import System.Environment (getArgs)
import Data.Time
import Data.Text.Time (parseISODateTime)

import Data.List (sortOn)
import qualified Data.TimeSeries as TS
import qualified Data.TimeSeries.Sessions as S


-- Find rains with highest intensity
-- To find when rain event start and stop we use find function from Sessions module
-- To classify rains we take max 6h intensity (sum of precipitation) during the event
-- Then we sort rains and print the heaviest ones to the console
--
main = do
    args <- getArgs
    let fn = head args
    ts <- TS.loadCSV TS.HasHeader parseISODateTime fn
    let ss = S.find (TS.seconds (6*3600)) (fmap (> 0.1) ts)
    let ts' = TS.rolling (TS.seconds (6*3600)) sum ts
    let xs = map (\s -> (s, maxIntensity s ts')) ss
    let xs' = filter (\x -> snd x > 20) xs
    let xs'' = sortOn snd xs'
    mapM print xs''


maxIntensity :: Ord a => S.Session -> TS.Series a -> a
maxIntensity (S.Session t1 t2) ts = maximum (TS.slice t1 t2 ts)