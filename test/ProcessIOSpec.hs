module ProcessIOSpec where

import Helm
import Test.Hspec


processIOSpec :: SpecWith ()
processIOSpec = do
    describe "Given an IO Operation and an update function" $ do
        it "should call the update with the IO result message" $ do
            model <- processIO update (Model { expected = 99 }) theAnswer
            expected model `shouldBe` 42
    describe "Given a list of IO Operation and an update function" $ do
        it "should call the list of IO operation and returns these list" $ do
            model <- processIOBulkParallel update (Model { expected = 99 }) ProcessedList [theNumber, theAnotherNumber]
            expected model `shouldBe` 2000


data Msg
  = Completed Int
  | ProcessedList [Int]

data Model = Model { expected :: Int }

update :: Msg -> Model -> Model
update msg model =
    case msg of
      Completed value ->
          Model { expected = value }
      ProcessedList list ->
          Model { expected = foldl (+) 0 list }


theAnswer :: IO Msg
theAnswer = do
    return $ Completed 42


theNumber :: IO Int
theNumber = do
    return $ 1993


theAnotherNumber :: IO Int
theAnotherNumber = do
    return $ 7
