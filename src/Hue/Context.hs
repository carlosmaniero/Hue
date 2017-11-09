module Hue.Context where

import Data.List

newtype HueContextManager context = HueContextManager
  { hueContexts :: [HueContext context]
  }

type ContextIdType = Int

type ContextType context = context

data (Eq context) =>
     HueContext context = HueContext
  { hueContextId :: ContextIdType
  , hueContext :: context
  } deriving (Eq, Show)

hueCreateContextManager :: HueContextManager context
hueCreateContextManager = HueContextManager {hueContexts = []}

hueRegisterContext ::
     Eq context
  => HueContextManager context
  -> context
  -> (HueContextManager context, HueContext context)
hueRegisterContext contextManager context =
  (HueContextManager {hueContexts = contexts}, newContext)
  where
    newContext =
      HueContext
      {hueContextId = length $ hueContexts contextManager, hueContext = context}
    contexts = newContext : hueContexts contextManager

hueIsSameId :: Eq context => ContextIdType -> HueContext context -> Bool
hueIsSameId contextId context = hueContextId context == contextId

hueGetContextById :: Eq context =>
     HueContextManager context -> ContextIdType -> Maybe (HueContext context)
hueGetContextById contextManager contextId =
  find (hueIsSameId contextId) (hueContexts contextManager)

hueHasContextChange :: Eq context =>
     HueContextManager context
  -> ContextIdType
  -> HueContext context
  -> Either Bool Bool
hueHasContextChange contextManager contextId context =
  case hueGetContextById contextManager contextId of
    Just storedContext -> Right $ storedContext /= context
    Nothing -> Left True
