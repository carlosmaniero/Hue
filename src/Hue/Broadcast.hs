module Hue.Broadcast
  ( HueBroadcast(..)
  , HueBroadcastWritter
  , hueCreateBroadcast
  , hueBroadcastAdapterWritter
  , HueBroadcastSource(..)
  ) where

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM
import Hue.Context

type HueBroadcastWritter msg = (msg -> IO ())

data HueBroadcastSource context msg
  = HueBroadcastApp (HueContext context)
                    msg
  | HueBroadcastAdapter context msg

data HueBroadcast context msg = HueBroadcast
  { hueBroadcastChannel :: TBQueue (HueBroadcastSource context msg)
  , hueBroadcastWritter :: HueBroadcastSource context msg -> IO ()
  , hueBroadcastReader :: IO (HueBroadcastSource context msg)
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

hueBroadcastAdapterWritter :: HueBroadcast context msg -> context -> msg -> IO ()
hueBroadcastAdapterWritter broadcast context msg =
  hueBroadcastWritter broadcast (HueBroadcastAdapter context msg)

broadcastWritter ::
     TBQueue (HueBroadcastSource context msg)
  -> HueBroadcastSource context msg
  -> IO ()
broadcastWritter channel source = do
  atomically $ writeTBQueue channel source
  return ()
