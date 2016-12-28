import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Control.Arrow (first)
import qualified Statistics.Sample as S
import qualified Data.Vector as V
import Data.Time
import Data.Text.Time (parseISODateTime)

import qualified Data.TimeSeries as TS


signal :: Num a => TS.Series a -> [(LocalTime, a)]
signal ts = map (first (utcToLocalTime utc)) (TS.toList ts)


main = do
    ts <- TS.loadCSV TS.HasHeader parseISODateTime "testdata/co2.csv"
    let y3 = TS.years 3
    let xs = TS.rolling y3 (S.mean . V.fromList) ts
    toFile def "out/co2_smooth.svg" $ do
        layout_title .= "CO2 level"
        setColors [opaque blue, opaque red]
        plot (line "CO2" [signal ts])
        plot (line "Average" [signal xs])
