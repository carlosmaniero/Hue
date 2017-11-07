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
                   , appUpdater :: context -> msg -> model -> (context, model, CmdType context msg)
                   , appCmd :: CmdType context msg
                   }


-- | 'CmdType' represent the type of command should be executed in the next loop iteration
--
-- * 'Cmd' receives a process to be executed
-- * 'CmdNone' do nothing
-- * 'CmdExit' stop the loop
data CmdType context msg = Cmd context ((msg -> IO ()) -> IO Task) | CmdNone | CmdExit


-- | 'hueStart' starts the loop with a given application
hueStart :: HueApplication msg model context result1 result2 result3 -> IO model
hueStart application = do
    channel <- atomically (newTBQueue 100)
    applicationLoop channel application


broadcastWritter :: TBQueue (context, msg) -> context -> msg -> IO()
broadcastWritter channel context msg = do
    atomically $ writeTBQueue channel (context, msg)
    return ()


getNextApplicationIteration :: TBQueue (context, msg) -> (HueApplication msg model context result1 result2 result3) -> IO model
getNextApplicationIteration channel application = do
    let broadcastReader = readTBQueue channel
    (context, msg) <- atomically broadcastReader
    let (nextContext, nextModel, nextCmd) = (appUpdater application) context msg (appModel application)
    let nextApplication = HueApplication { appModel = nextModel
                                         , appUpdater = appUpdater application
                                         , appCmd = nextCmd
                                         }
    applicationLoop channel nextApplication



applicationLoop :: TBQueue (context, msg) -> (HueApplication msg model context result1 result2 result3) -> IO model
applicationLoop channel application =
    case appCmd application of
      Cmd context cmd ->
        (do
            cmd (broadcastWritter channel context)
            getNextApplicationIteration channel application
        )
      CmdNone ->
        getNextApplicationIteration channel application
      CmdExit ->
        return $ appModel application
