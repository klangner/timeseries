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
    let startTime = UTCTime (fromGregorian 1965 1 1) 0
    let xs = TS.resample startTime (TS.years 1) ts
    toFile def "dist/resample.svg" $ do
        layout_title .= fn
        setColors [opaque blue, opaque red]
        plot (line "Original series" [signal ts])
        plot (line "Yearly average" [signal xs])
    putStrLn "Plot saved to: dist/resample.svg"
    createProcess (shell "firefox dist/resample.svg")
