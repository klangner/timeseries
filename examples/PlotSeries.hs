import System.Environment (getArgs)
import Control.Arrow (first)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time
import System.Process
import qualified Data.Vector as V

import Data.Text.Time (parseISODateTime)
import qualified Data.TimeSeries as TS
import qualified Data.TimeSeries.IO.CSV as CSV


main = do
    args <- getArgs
    let fn = head args
    ts <- CSV.load CSV.HasHeader parseISODateTime fn
    toFile def "dist/plot-series.svg" $ do
        layout_title .= fn
        setColors [opaque blue, opaque red]
        let xs = V.zip (TS.index ts) (TS.values ts)
        plot (line "" [V.toList xs])
    putStrLn "Plot saved to: dist/plot-series.svg"
    createProcess (shell "firefox dist/plot-series.svg")
