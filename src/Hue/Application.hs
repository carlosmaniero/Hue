module Hue.Application
    ( HueApplication(..)
    , hueStart
    , CmdType(..)
    )
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
data HueApplication msg model context result1 result2 result3 =
    HueApplication { appModel :: model
                   , appUpdater :: msg -> model -> (model, CmdType msg)
                   , appCmd :: CmdType msg
                   , appContext :: context
                   }


-- | 'CmdType' represent the type of command should be executed in the next loop iteration
--
-- * 'Cmd' receives a process to be executed
-- * 'CmdNone' do nothing
-- * 'CmdExit' stop the loop
data CmdType msg = Cmd ((msg -> IO ()) -> IO Task) | CmdNone | CmdExit


-- | 'hueStart' starts the loop with a given application
hueStart :: HueApplication msg model context result1 result2 result3 -> IO model
hueStart application = do
    channel <- atomically (newTBQueue 100)
    applicationLoop channel application


broadcastWritter :: TBQueue msg -> msg -> IO()
broadcastWritter channel msg = do
    atomically $ writeTBQueue channel msg
    return ()


getNextApplicationIteration :: TBQueue msg -> (HueApplication msg model context result1 result2 result3) -> IO model
getNextApplicationIteration channel application = do
    let broadcastReader = readTBQueue channel
    msg <- atomically broadcastReader
    let (nextModel, nextCmd) = (appUpdater application) msg (appModel application)
    let nextApplication = HueApplication { appModel = nextModel
                                         , appUpdater = (appUpdater application)
                                         , appCmd = nextCmd
                                         , appContext = (appContext application)
                                         }
    applicationLoop channel nextApplication



applicationLoop :: TBQueue msg -> (HueApplication msg model context result1 result2 result3) -> IO model
applicationLoop channel application =
    case appCmd application of
      Cmd cmd ->
        (do
            cmd (broadcastWritter channel)
            getNextApplicationIteration channel application
        )
      CmdNone ->
        getNextApplicationIteration channel application
      CmdExit ->
        return $ appModel application
