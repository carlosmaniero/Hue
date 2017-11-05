{-# LANGUAGE PartialTypeSignatures #-}
module ApplicationSpec where

import Hue.Application
import Hue.Process
import Control.Concurrent
import Control.Concurrent.MVar
import Test.Hspec
import Test.QuickCheck
import Control.Exception
import Control.Exception.Base


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
                points model `shouldBe` 55

            it "should never die if the CmdExit is not returned" $ do
                channel <- newEmptyMVar

                tid <- forkIO $ (
                            do
                                result <- try (
                                                hueStart HueApplication { appModel = model
                                                                        , appUpdater = update
                                                                        , appCmd = CmdNone
                                                                        }
                                               ) :: IO (Either BlockedIndefinitelyOnSTM Model)

                                case result of
                                    Left ex  ->
                                        putMVar channel True
                                    Right val ->
                                        putMVar channel True
                         )

                threadDelay 500000
                killThread tid
                isEmpty <- isEmptyMVar channel
                isEmpty `shouldBe` True

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
              cmd = if result == 55
                       then CmdExit
                    else CmdNone
      Minus num ->
          (model { points = num - (points model) }, CmdNone)
      ChangeName userName ->
          (model { name = userName }, cmd)
          where cmd = if userName == "Hue"
                         then sumNum 10
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
