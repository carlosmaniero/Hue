{-# LANGUAGE PartialTypeSignatures #-}
module ApplicationSpec where

import Hue.Application
import Hue.Process
import Test.Hspec
import Test.QuickCheck


processIOSpec :: SpecWith ()
processIOSpec = do
    describe "Given an updater, a model and a start command" $ do
        let model = Model { name = "", points = 0}
        describe "When the start command is name request change" $ do
            it "should update the model with the new name and the total of points" $ do
                model <- hueStart HueApplication { appModel = model
                                                 , appUpdater = update
                                                 , appCmd = setNameCmd "Hue"
                                                 }
                name model `shouldBe` "Hue"
                points model `shouldBe` 1


data Msg = Sum Int | Minus Int | ChangeName String

data Model = Model { name :: String
                   , points :: Int }

update :: Msg -> Model -> (Model, CmdType Msg _)
update msg model =
    case msg of
      Sum num ->
          (model { points = result }, cmd)
          where
              result = num + (points model)
              cmd = CmdExit
      Minus num ->
          (model { points = num - (points model) }, CmdNone)
      ChangeName userName ->
          (model { name = userName }, cmd)
          where cmd = if userName == "Hue"
                         then sumNum 1
                      else CmdNone


setNameCmd :: String -> CmdType Msg String
setNameCmd theName =
    Cmd $ Process { processType = ProcessOnly (do return theName) ChangeName
                  , processCancellable = Nothing }

sumIO :: Int -> IO Int
sumIO num = do
    return num

sumNum :: Int -> CmdType Msg Int
sumNum total =
    Cmd $ Process { processType = ProcessMany (map sumIO $ [0..total]) Sum
                   , processCancellable = Nothing }
