module ApplicationSpec where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Exception.Base
import Hue.Application
import Hue.Broadcast
import Hue.Context
import Hue.Process
import Test.Hspec
import Test.QuickCheck

processIOSpec :: SpecWith ()
processIOSpec =
  describe "Given an updater, a model and a start command" $ do
    let model = Model {name = "", points = 0}
    describe "When the start command is name request change" $ do
      it "should update the model with the new name and the total of points" $ do
        model <-
          hueStart
            HueApplication
            { appModel = model
            , appUpdater = update
            , appMainContext = RootContext
            , appInit = setNameProcess "Hue"
            }
        name model `shouldBe` "Hue"
        points model `shouldBe` 55
      it "should never die if the CmdExit is not returned" $ do
        channel <- newEmptyMVar
        tid <-
          forkIO
            (do result <-
                  try
                    (hueStart
                       HueApplication
                       { appModel = model
                       , appMainContext = RootContext
                       , appUpdater = update
                       , appInit = hueProcessDoesNothing
                       }) :: IO (Either BlockedIndefinitelyOnSTM Model)
                case result of
                  Left ex -> putMVar channel True
                  Right val -> putMVar channel True)
        threadDelay 500000
        killThread tid
        isEmpty <- isEmptyMVar channel
        isEmpty `shouldBe` True

data Msg
  = Sum Int
  | Minus Int
  | ChangeName String

data Model = Model
  { name :: String
  , points :: Int
  }

data Context =
  RootContext
  deriving (Eq)

update ::
     HueContext Context
  -> Msg
  -> Model
  -> (HueContext Context, Model, CmdType (HueContext Context) Msg)
update context msg model =
  case msg of
    Sum num -> (context, model {points = result}, cmd)
      where result = num + points model
            cmd =
              if result == 55
                then CmdExit
                else CmdNone
    Minus num -> (context, model {points = num - points model}, CmdNone)
    ChangeName userName -> (context, model {name = userName}, cmd)
      where cmd =
              if userName == "Hue"
                then sumNum context 10
                else CmdNone

setNameProcess :: String -> HueBroadcastWritter Msg -> IO Task
setNameProcess theName =
  startProcess
    Process
    { processType = ProcessOnly (return theName) ChangeName
    , processCancellable = Nothing
    }

sumIO :: Int -> IO Int
sumIO = return

sumNum :: HueContext Context -> Int -> CmdType (HueContext Context) Msg
sumNum context total =
  Cmd context $
  startProcess
    Process
    { processType = ProcessMany (map sumIO [0 .. total]) Sum
    , processCancellable = Nothing
    }
