import Test.Hspec
import Test.QuickCheck
import ProcessIOSpec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "Helm.Process" processIOSpec
