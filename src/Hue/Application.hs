module Hue.Application
  ( HueApplication(..)
  , hueStart
  , CmdType(..)
  ) where

import Hue.Broadcast
import Hue.Context
import Hue.Process

-- -----------------------------------------------------------------------------
-- Application
-- | Here is where it all begins. The application is what will be executed in
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
data Eq context =>
     HueApplication msg model context result1 result2 result3 = HueApplication
  { appModel :: model
  , appUpdater :: HueContext context -> msg -> model -> ( HueContext context
                                                        , model
                                                        , CmdType (HueContext context) msg)
  , appMainContext :: context
  , appInit :: HueBroadcastWritter msg -> IO Task
  }

data Eq context =>
     HueApplicationIteration msg model context result1 result2 result3 = HueApplicationIteration
  { iterModel :: model
  , iterContextManager :: HueContextManager context
  , iterUpdater :: HueContext context -> msg -> model -> ( HueContext context
                                                         , model
                                                         , CmdType (HueContext context) msg)
  , iterCmd :: CmdType (HueContext context) msg
  , iterSource :: HueBroadcastSource
  }

-- | 'CmdType' represent the type of command should be executed in the next loop iteration
--
-- * 'Cmd' receives a process to be executed
-- * 'CmdNone' do nothing
-- * 'CmdExit' stop the loop
data CmdType context msg
  = Cmd context
        (HueBroadcastWritter msg -> IO Task)
  | CmdNone
  | CmdExit

-- | 'hueStart' starts the loop with a given application
hueStart ::
     Eq context
  => HueApplication msg model context result1 result2 result3
  -> IO model
hueStart application = do
  broadcast <- hueCreateBroadcast
  applicationLoop broadcast iteration
  where
    blankContextManager = hueCreateContextManager
    (contextManager, contextInstance) =
      hueRegisterContext blankContextManager (appMainContext application)
    iteration =
      HueApplicationIteration
      { iterModel = appModel application
      , iterUpdater = appUpdater application
      , iterContextManager = contextManager
      , iterCmd = Cmd contextInstance (appInit application)
      , iterSource = HueBroadcastApp
      }

getNextApplicationIteration ::
     Eq context
  => HueBroadcast context msg
  -> HueApplicationIteration msg model context result1 result2 result3
  -> IO model
getNextApplicationIteration broadcast iteration = do
  (source, context, msg) <- hueBroadcastReader broadcast
  let (nextContext, nextModel, nextCmd) =
        iterUpdater iteration context msg (iterModel iteration)
  let nextIteration =
        HueApplicationIteration
        { iterModel = nextModel
        , iterUpdater = iterUpdater iteration
        , iterContextManager = iterContextManager iteration
        , iterCmd = nextCmd
        , iterSource = HueBroadcastApp
        }
  applicationLoop broadcast nextIteration

applicationLoop ::
     Eq context
  => HueBroadcast context msg
  -> HueApplicationIteration msg model context result1 result2 result3
  -> IO model
applicationLoop broadcast iteration =
  case iterCmd iteration of
    Cmd context cmd -> do
      cmd (hueBroadcastWritter broadcast (iterSource iteration) context)
      getNextApplicationIteration broadcast iteration
    CmdNone -> getNextApplicationIteration broadcast iteration
    CmdExit -> return $ iterModel iteration
