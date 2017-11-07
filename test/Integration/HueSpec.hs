import ApplicationSpec
import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main =
  hspec $
  describe "Integration Tests" $ describe "Hue.Application" processIOSpec
