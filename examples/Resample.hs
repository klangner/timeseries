import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import qualified Statistics.Sample as S
import qualified Data.Vector as V
import Data.Time
import Data.Text.Time (parseISODateTime)

import qualified Data.TimeSeries as TS


signal :: Num a => TS.Series a -> [(LocalTime, a)]
signal ts = map (\(x, y) -> (utcToLocalTime utc x, y)) (TS.toList ts)


main = do
    ts <- TS.loadCSV TS.HasHeader parseISODateTime "testdata/co2.csv"
    let startTime = UTCTime (fromGregorian 1965 1 1) 0
    let xs = TS.resample startTime (TS.years 1) (S.mean . V.fromList) ts
    toFile def "temp/co2_resample.svg" $ do
        layout_title .= "CO2 level"
        setColors [opaque blue, opaque red]
        plot (line "CO2" [signal ts])
        plot (line "Yearly average" [signal xs])
