import System.Environment (getArgs)
import Control.Arrow (first)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time
import System.Process

import Data.Text.Time (parseISODateTime)
import qualified Data.TimeSeries as TS


signal :: Num a => TS.Series a -> [(LocalTime, a)]
signal ts = map (first (utcToLocalTime utc)) (TS.toList ts)

main = do
    args <- getArgs
    let fn = head args
    ts <- TS.loadCSV TS.HasHeader parseISODateTime fn
    toFile def "dist/plot-series.svg" $ do
        layout_title .= fn
        setColors [opaque blue, opaque red]
        plot (line "" [signal ts])
    putStrLn "Plot saved to: dist/plot-series.svg"
    createProcess (shell "firefox dist/plot-series.svg")
