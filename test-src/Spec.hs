
import Prelude
import Test.Hspec
import qualified Data.TimeSeriesSpec
import qualified Data.TimeSeries.SessionsSpec
import qualified Data.TimeSeries.StatsSpec
import qualified Data.TimeSeries.TimeSpec
import qualified Data.TimeSeries.IO.CSVSpec


main :: IO ()
main = hspec $ do
  describe "TimeSeries" Data.TimeSeriesSpec.spec
  describe "Sessions" Data.TimeSeries.SessionsSpec.spec
  describe "Statistcs" Data.TimeSeries.StatsSpec.spec
  describe "Time" Data.TimeSeries.TimeSpec.spec
  describe "CSV" Data.TimeSeries.IO.CSVSpec.spec
