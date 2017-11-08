import Control.Exception (evaluate)
import ProcessIOSpec
import ContextSpec
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $
    describe "Unit Tests" $ do
        describe "Hue.Process" processIOSpec
        describe "Hue.Context" contextSpec
