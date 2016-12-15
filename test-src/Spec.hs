import Test.Hspec
import qualified Data.TimeSeries.SeriesSpec
import qualified Data.TimeSeries.StatsSpec
import qualified Data.TimeSeries.IO.CSVSpec


main :: IO ()
main = hspec $ do
  describe "Series" Data.TimeSeries.SeriesSpec.spec
  describe "Statistcs" Data.TimeSeries.StatsSpec.spec
  describe "CSV" Data.TimeSeries.IO.CSVSpec.spec
