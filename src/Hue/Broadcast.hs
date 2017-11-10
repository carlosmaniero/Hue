module Hue.Broadcast
  ( HueBroadcast(..)
  , HueBroadcastWritter
  , hueCreateBroadcast
  , HueBroadcastSource(..)
  ) where

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM
import Hue.Context

type HueBroadcastWritter msg = (msg -> IO ())

data HueBroadcastSource
  = HueBroadcastApp
  | HueBroadcastAdapter

data HueBroadcast context msg = HueBroadcast
  { hueBroadcastChannel :: TBQueue (HueBroadcastSource, HueContext context, msg)
  , hueBroadcastWritter :: HueBroadcastSource -> HueContext context -> msg -> IO ()
  , hueBroadcastReader :: IO (HueBroadcastSource, HueContext context, msg)
  }

hueCreateBroadcast :: IO (HueBroadcast context msg)
hueCreateBroadcast = do
  channel <- atomically (newTBQueue 100)
  return
    HueBroadcast
    { hueBroadcastChannel = channel
    , hueBroadcastWritter = broadcastWritter channel
    , hueBroadcastReader = atomically $ readTBQueue channel
    }

broadcastWritter ::
     TBQueue (HueBroadcastSource, HueContext context, msg)
  -> HueBroadcastSource
  -> HueContext context
  -> msg
  -> IO ()
broadcastWritter channel source context msg = do
  atomically $ writeTBQueue channel (source, context, msg)
  return ()
