import Control.Arrow (first)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time
import Data.Text.Time (parseISODateTime)

import qualified Data.TimeSeries as TS


signal :: Num a => TS.Series a -> [(LocalTime, a)]
signal ts = map (first (utcToLocalTime utc)) (TS.toList ts)


main = do
    ts <- TS.loadCSV TS.HasHeader parseISODateTime "testdata/co2.csv"
    let startTime = UTCTime (fromGregorian 1965 1 1) 0
    let xs = TS.resample startTime (TS.years 1) ts
    toFile def "temp/co2_resample.svg" $ do
        layout_title .= "CO2 level"
        setColors [opaque blue, opaque red]
        plot (line "CO2" [signal ts])
        plot (line "Yearly average" [signal xs])
