import System.Environment (getArgs)
import Control.Arrow (first)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time
import qualified Statistics.Sample as S
import qualified Data.Vector as V
import System.Process

import Data.Text.Time (parseISODateTime)
import qualified Data.TimeSeries as TS


signal :: Num a => TS.Series a -> [(LocalTime, a)]
signal ts = map (first (utcToLocalTime utc)) (TS.toList ts)


main = do
    args <- getArgs
    let fn = head args
    ts <- TS.loadCSV TS.HasHeader parseISODateTime fn
    let y3 = TS.years 3
    let xs = TS.rolling y3 (S.mean . V.fromList) ts
    toFile def "dist/smooth.svg" $ do
        layout_title .= fn
        setColors [opaque blue, opaque red]
        plot (line "Orignal" [signal ts])
        plot (line "Smoothed" [signal xs])
    putStrLn "Plot saved to: dist/smooth.svg"
    createProcess (shell "firefox dist/smooth.svg")
