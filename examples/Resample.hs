import System.Environment (getArgs)
import Control.Arrow (first)
import Graphics.Rendering.Chart.Easy hiding (argument)
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time
import System.Process
import Options.Applicative
import Data.Semigroup ((<>))

import Data.Text.Time (parseISODateTime)
import qualified Data.TimeSeries as TS


-- Command line params
data Params = Params
  { resolution  :: String
  , fileName  :: String }
  deriving (Show)

-- Command line parser
params :: Parser Params
params = Params
     <$> strOption
         ( long "time"
        <> metavar "<Y | M | D | H>"
        <> help "Resampling resolution." )
     <*> argument str (metavar "FILE")

-- Convert string representation of time into TimeResolution
convertTime :: String -> TS.TimeResolution
convertTime "Y" = TS.years 1
convertTime "M" = TS.months 1
convertTime "D" = TS.days 1
convertTime "H" = TS.seconds 3600


-- Main function. Parses command line arguments and calls drawResampled for Time Series processing
main :: IO ()
main = do
    ps <- execParser opts
    ts <- TS.loadCSV TS.HasHeader parseISODateTime (fileName ps)
    drawResampled ts (convertTime (resolution ps))
    where
        opts = info (helper <*> params) ( fullDesc <> progDesc "Resampling time series" )


-- Convert Time series to format expected by Chart library
signal :: Num a => TS.Series a -> [(LocalTime, a)]
signal ts = map (first (utcToLocalTime utc)) (TS.toList ts)


-- Take time series with time resolutions and creates chart with original and modified series
drawResampled :: TS.Series Double -> TS.TimeResolution -> IO ()
drawResampled ts dt = do
    args <- getArgs
    let fn = head args
    let startTime = UTCTime (fromGregorian 1965 1 1) 0
    let xs = TS.resample startTime dt ts
    toFile def "dist/resample.svg" $ do
        layout_title .= fn
        setColors [opaque blue, opaque red]
        plot (line "Original series" [signal ts])
        plot (line "Resampled series" [signal xs])
    putStrLn "Plot saved to: dist/resample.svg"
    _ <- createProcess (shell "firefox dist/resample.svg")
    return ()
