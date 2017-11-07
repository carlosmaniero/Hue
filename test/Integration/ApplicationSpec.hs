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
                                                 , appCmd = setNameCmd RootContext "Hue"
                                                 , appContext = RootContext
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
                                                                        , appContext = RootContext
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

data Context = RootContext


update :: Context -> Msg -> Model -> (Context, Model, CmdType Context Msg)
update context msg model =
    case msg of
        Sum num ->
            (context, model { points = result }, cmd)
            where
                result = num + (points model)
                cmd =
                    if result == 55
                        then CmdExit
                    else CmdNone
        Minus num ->
            (context, model { points = num - (points model) }, CmdNone)
        ChangeName userName ->
            (context, model { name = userName }, cmd)
            where cmd =
                        if userName == "Hue"
                           then sumNum context 10
                        else CmdNone


setNameCmd :: Context -> String -> CmdType Context Msg
setNameCmd context theName =
    Cmd context $ startProcess Process { processType = ProcessOnly (do return theName) ChangeName
                                 , processCancellable = Nothing }

sumIO :: Int -> IO Int
sumIO num = do
    return num

sumNum :: Context -> Int -> CmdType Context Msg
sumNum context total =
    Cmd context $ startProcess Process { processType = ProcessMany (map sumIO $ [0..total]) Sum
                               , processCancellable = Nothing }
