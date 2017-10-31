module ProcessIOSpec where

import Helm
import Test.Hspec


processIOSpec :: SpecWith ()
processIOSpec = do
    let initModel = Model { expected = 0, updates = 0 }
    describe "Given an IO Operation and an update function" $ do
        it "should call the update with the IO result message" $ do
            model <- processIO update initModel theAnswer
            expected model `shouldBe` 42
            updates model `shouldBe` 1
    describe "Given a list of IO Operation and an update function" $ do
        describe "When I expect the parallel operation to update the full list of result" $ do
            it "should process all the list of IO operation" $ do
                model <- processIOBulkParallel update initModel ProcessedList [theNumber, theAnotherNumber]
                expected model `shouldBe` 2000
                updates model `shouldBe` 1
            it "should do nothing given a empty list" $ do
                model <- processIOBulkParallel update initModel ProcessedList []
                expected model `shouldBe` 0
                updates model `shouldBe` 0
        describe "When I expect the parallel operation to update each operation" $ do
            it "should process the list and return as soon as the operation is done" $ do
                model <- processIOParallel update initModel Completed [theNumber, theAnotherNumber]
                updates model `shouldBe` 2
                expected model `shouldBe` 2000
            it "should do nothing given a empty list" $ do
                model <- processIOParallel update initModel Completed []
                expected model `shouldBe` 0
                updates model `shouldBe` 0


data Msg
  = Completed Int
  | ProcessedList [Int]

data Model = Model { expected :: Int
                   , updates  :: Int }

update :: Msg -> Model -> Model
update msg model =
    let
        calls = 1 + updates model
    in
    case msg of
      Completed value ->
          Model { expected = value + expected model, updates = calls }
      ProcessedList list ->
          Model { expected = foldl (+) 0 list, updates = calls }


theAnswer :: IO Msg
theAnswer = do
    return $ Completed 42


theNumber :: IO Int
theNumber = do
    return $ 1993


theAnotherNumber :: IO Int
theAnotherNumber = do
    return $ 7
