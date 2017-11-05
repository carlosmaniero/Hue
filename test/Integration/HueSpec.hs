import Test.Hspec
import Test.QuickCheck
import ApplicationSpec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "Integration Tests" $ do
        describe "Hue.Application" processIOSpec
