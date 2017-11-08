module Hue.Broadcast
  ( HueBroadcast(..)
  , HueBroadcastWritter
  , hueCreateBroadcast
  ) where

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM

type HueBroadcastWritter msg = (msg -> IO ())

data HueBroadcast context msg = HueBroadcast
  { hueBroadcastChannel :: TBQueue (context, msg)
  , hueBroadcastWritter :: context -> msg -> IO ()
  , hueBroadcastReader :: IO (context, msg)
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

broadcastWritter :: TBQueue (context, msg) -> context -> msg -> IO ()
broadcastWritter channel context msg = do
  atomically $ writeTBQueue channel (context, msg)
  return ()
