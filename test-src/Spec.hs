
import Prelude
import Test.Hspec
import qualified Data.TimeSeries.IndexSpec
import qualified Data.TimeSeries.IO.CSVSpec


main :: IO ()
main = hspec $ do
  describe "DateTimeIndex" Data.TimeSeries.IndexSpec.spec
  describe "CSV" Data.TimeSeries.IO.CSVSpec.spec
