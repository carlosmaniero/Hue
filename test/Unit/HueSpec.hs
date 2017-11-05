import Test.Hspec
import Test.QuickCheck
import ProcessIOSpec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "Unit Tests" $ do
        describe "Hue.Process" processIOSpec
