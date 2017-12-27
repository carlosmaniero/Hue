import Test.Hspec
import HueIterationSpec


main :: IO ()
main = hspec $
  describe "Unit Tests" $ do
    describe "Hue.Iteration" hueIterationSpec
