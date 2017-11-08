module Hue.Context where

import Data.List

newtype HueContextManager context = HueContextManager
  { hueContexts :: [HueContext context]
  }

type ContextIdType = Int

data HueContext context = HueContext
  { hueContextId :: ContextIdType
  , hueContext :: context
  } deriving (Eq, Show)

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

hueIsSameId :: ContextIdType -> HueContext hue -> Bool
hueIsSameId contextId context = hueContextId context == contextId

hueGetContextById ::
     HueContextManager context -> ContextIdType -> Maybe (HueContext context)
hueGetContextById contextManager contextId =
    find (hueIsSameId contextId) (hueContexts contextManager)
