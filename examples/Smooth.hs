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
    let y3 = 3 * 365 * 24 * 3600
    let xs = TS.rolling y3 (S.mean . V.fromList) ts
    toFile def "temp/co2_smooth.svg" $ do
        layout_title .= "CO2 level"
        setColors [opaque blue, opaque red]
        plot (line "CO2" [signal ts])
        plot (line "Average" [signal xs])
