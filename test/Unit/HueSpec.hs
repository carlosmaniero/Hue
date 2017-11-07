import Control.Exception (evaluate)
import ProcessIOSpec
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ describe "Unit Tests" $ describe "Hue.Process" processIOSpec
