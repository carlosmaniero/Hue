module Hue.Broadcast
  ( HueBroadcast(..)
  , HueBroadcastWritter
  , hueCreateBroadcast
  ) where

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM
import Hue.Context

type HueBroadcastWritter msg = (msg -> IO ())

data HueBroadcast context msg = HueBroadcast
  { hueBroadcastChannel :: TBQueue (HueContext context, msg)
  , hueBroadcastWritter :: HueContext context -> msg -> IO ()
  , hueBroadcastReader :: IO (HueContext context, msg)
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
     TBQueue (HueContext context, msg) -> HueContext context -> msg -> IO ()
broadcastWritter channel context msg = do
  atomically $ writeTBQueue channel (context, msg)
  return ()
