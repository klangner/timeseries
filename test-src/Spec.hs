
import Prelude
import Test.Hspec
import qualified Data.TimeSeries.IndexSpec


main :: IO ()
main = hspec $ do
  describe "DateTimeIndex" Data.TimeSeries.IndexSpec.spec
