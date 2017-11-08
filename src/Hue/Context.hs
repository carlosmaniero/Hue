module Hue.Context where

newtype HueContextManager context = HueContextManager
  { hueContexts :: [HueContext context]
  }

data HueContext context = HueContext
  { hueContextId :: Int
  , hueContext :: context
  }

hueCreateContextManager :: HueContextManager context
hueCreateContextManager = HueContextManager {hueContexts = []}

hueRegisterContext ::
     HueContextManager context
  -> context
  -> (HueContextManager context, HueContext context)
hueRegisterContext contextManager context =
  (HueContextManager {hueContexts = contexts}, newContext)
  where
    newContext =
      HueContext
      {hueContextId = length $ hueContexts contextManager, hueContext = context}
    contexts = newContext : hueContexts contextManager
