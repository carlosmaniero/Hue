import Test.Hspec
import HueIterationSpec
import HueRunnerSpec


main :: IO ()
main = hspec $
  describe "Unit Tests" $ do
    describe "Hue.Iteration" hueIterationSpec
    describe "Hue.Runner" hueRunnerSpec
