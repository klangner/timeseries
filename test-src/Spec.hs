import Test.Hspec
import qualified Data.TimeSeries.SeriesSpec


main :: IO ()
main = hspec $ do
  describe "Series" Data.TimeSeries.SeriesSpec.spec
