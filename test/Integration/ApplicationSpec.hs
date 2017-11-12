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
    describe "When the start command is a name request change" $ do
      it "should update the model with the new name and the total of points" $ do
        modelChannel <- newEmptyMVar
        runApplicationAsync
          modelChannel
          HueApplication
          { appModel = model
          , appUpdater = update
          , appMainContext = RootContext
          , appInit = setNameProcess "Hue"
          , appAdapters = []
          }
        modelResult <- takeMVar modelChannel
        case modelResult of
          Right model -> do
            name model `shouldBe` "Hue"
            points model `shouldBe` 55
      it "should never die if the CmdExit is not returned" $ do
        channel <- newEmptyMVar
        tid <-
          runApplicationAsync
            channel
            HueApplication
            { appModel = model
            , appMainContext = RootContext
            , appUpdater = update
            , appInit = hueProcessDoesNothing
            , appAdapters = []
            }
        threadDelay 500000
        killThread tid
        isEmpty <- isEmptyMVar channel
        isEmpty `shouldBe` True
    describe "Given an adapter" $
      it "should send a message from the update" $ do
        adapterChannel <- newEmptyMVar
        modelChannel <- newEmptyMVar
        runApplicationAsync
          modelChannel
          HueApplication
          { appModel = model
          , appUpdater = update
          , appMainContext = RootContext
          , appInit = setNameProcess "HueBr"
          , appAdapters = [channelAdapter adapterChannel]
          }
        putMVar adapterChannel (AdapterContext, Sum 55)
        modelResult <- takeMVar modelChannel
        case modelResult of
          Right model -> points model `shouldBe` 55

data Msg
  = Sum Int
  | Minus Int
  | ChangeName String

data Model = Model
  { name :: String
  , points :: Int
  }

data Context
  = RootContext
  | AdapterContext
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

runApplicationAsync ::
     MVar (Either BlockedIndefinitelyOnSTM Model)
  -> HueApplication Msg Model Context
  -> IO ThreadId
runApplicationAsync channel application =
  forkIO $ do
    result <-
      try (hueStart application) :: IO (Either BlockedIndefinitelyOnSTM Model)
    putMVar channel result

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

channelAdapter :: MVar (Context, Msg) -> (Context -> Msg -> IO ()) -> IO Task
channelAdapter channel broadcast =
  forkIO $ do
    (context, msg) <- takeMVar channel
    broadcast context msg
    channelAdapter channel broadcast
    return ()
