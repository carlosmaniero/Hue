module Hue.Application
where


import Hue.Process
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent
import Control.Concurrent.STM.TBQueue


-- -----------------------------------------------------------------------------
-- Application


-- | Here is were it all begins. The application is what will be executed in
-- Hue.
--
-- * 'appModel' can be anything and it represents the state of your application.
-- * 'appUpdater' is the function that will be called when an command is completed
-- * 'appCmd' is the cmd that will be executed in the application iteration
--
-- A 'msg' should be anything that should be used to know who sends the 'msg' and
-- what to do with it. But normally is a type like it;
--
-- > data Msg = AdoptCats Int
-- >          | BuyCatFood
-- >          | PlayWithCats
--
-- The model is the state of your application. Normally, the model is a record.
data HueApplication msg model result1 result2 result3 = HueApplication { appModel :: model
                                                                       , appUpdater :: msg -> model -> (model, CmdType msg result1)
                                                                       , appCmd :: CmdType msg result3
                                                                       }

data CmdType msg result = Cmd (Process msg result) | CmdNone | CmdExit


hueStart :: HueApplication msg model result1 result2 result3 -> IO model
hueStart application = do
    channel <- atomically (newTBQueue 100)
    startApplication channel application


broadcastWritter :: TBQueue msg -> msg -> IO()
broadcastWritter channel msg = do
    atomically $ writeTBQueue channel msg
    return ()


startApplication :: TBQueue msg -> (HueApplication msg model result1 result2 result3) -> IO model
startApplication channel application =
    case appCmd application of
      Cmd cmd ->
          (do
              let broadcastReader = readTBQueue channel
              startProcess (broadcastWritter channel) cmd
              msg <- atomically broadcastReader
              let (nextModel, nextCmd) = (appUpdater application) msg (appModel application)
              let nextApplication = HueApplication { appModel = nextModel
                                                   , appUpdater = (appUpdater application)
                                                   , appCmd = nextCmd
                                                   }
              startApplication channel nextApplication
          )
      CmdNone ->
          (do
            let broadcastReader = readTBQueue channel
            msg <- atomically broadcastReader
            let (nextModel, nextCmd) = (appUpdater application) msg (appModel application)
            let nextApplication = HueApplication { appModel = nextModel
                                                 , appUpdater = (appUpdater application)
                                                 , appCmd = nextCmd
                                                 }
            startApplication channel nextApplication
          )
      CmdExit ->
          return $ appModel application
