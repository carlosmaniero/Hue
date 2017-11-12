module Hue.Application
  ( HueApplication(..)
  , hueStart
  , CmdType(..)
  ) where

import Hue.Adapter
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
     HueApplication msg model context = HueApplication
  { appModel :: model
  , appUpdater :: HueContext context -> msg -> model -> ( HueContext context
                                                        , model
                                                        , CmdType (HueContext context) msg)
  , appMainContext :: context
  , appInit :: HueBroadcastWritter msg -> IO Task
  , appAdapters :: [HueAdapter context msg]
  }

data Eq context =>
     HueApplicationIteration msg model context = HueApplicationIteration
  { iterModel :: model
  , iterContextManager :: HueContextManager context
  , iterUpdater :: HueContext context -> msg -> model -> ( HueContext context
                                                         , model
                                                         , CmdType (HueContext context) msg)
  , iterCmd :: CmdType (HueContext context) msg
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

hueRegisterAdapters ::
     [HueAdapter context msg] -> HueBroadcast context msg -> IO ()
hueRegisterAdapters adapters broadcast = do
  let broadcastWritter = hueBroadcastAdapterWritter broadcast
  mapM_ (\adapter -> adapter broadcastWritter) adapters

-- | 'hueStart' starts the loop with a given application
hueStart :: Eq context => HueApplication msg model context -> IO model
hueStart application = do
  broadcast <- hueCreateBroadcast
  hueRegisterAdapters (appAdapters application) broadcast
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
      }

getNextApplicationIteration ::
     Eq context
  => HueBroadcast context msg
  -> HueApplicationIteration msg model context
  -> IO model
getNextApplicationIteration broadcast iteration = do
  source <- hueBroadcastReader broadcast
  let (nextContextManager, context, msg) =
        case source of
          HueBroadcastApp context msg ->
            (iterContextManager iteration, context, msg)
          HueBroadcastAdapter contextToRegister msg ->
            (contextManager, context, msg)
            where (contextManager, context) =
                    hueRegisterContext
                      (iterContextManager iteration)
                      contextToRegister
  let (nextContext, nextModel, nextCmd) =
        iterUpdater iteration context msg (iterModel iteration)
  let nextIteration =
        HueApplicationIteration
        { iterModel = nextModel
        , iterUpdater = iterUpdater iteration
        , iterContextManager = nextContextManager
        , iterCmd = nextCmd
        }
  applicationLoop broadcast nextIteration

applicationLoop ::
     Eq context
  => HueBroadcast context msg
  -> HueApplicationIteration msg model context
  -> IO model
applicationLoop broadcast iteration =
  case iterCmd iteration of
    Cmd context cmd -> do
      cmd (hueBroadcastWritter broadcast . HueBroadcastApp context)
      getNextApplicationIteration broadcast iteration
    CmdNone -> getNextApplicationIteration broadcast iteration
    CmdExit -> return $ iterModel iteration
