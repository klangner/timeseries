
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Default.Class
import Control.Lens
import Data.Text.Time (parseISODateTime)
import qualified Data.TimeSeries as TS


chart = toRenderable layout
  where
    values = [ ("Mexico City",19.2,e), ("Mumbai",12.9,e)
             , ("Sydney",4.3,e), ("London",8.3,e), ("New York",8.2,e1) ]
    e = 0
    e1 = 25
    pitem (s,v,o) = pitem_value .~ v
                  $ pitem_label .~ s
                  $ pitem_offset .~ o
                  $ def

    layout = pie_title .~ "Relative Population"
           $ pie_plot . pie_data .~ map pitem values
           $ def


main :: IO ()
main = do
    ts <- TS.loadCSV TS.NoHeader parseISODateTime "../testdata/co2.csv"
    renderableToFile def "../temp/co2.svg" chart
