module Hue.Context
  ( HueContextManager(..)
  , HueContext(..)
  , hueCreateContextManager
  , hueRegisterContext
  , hueHasContextChange
  , hueGetContextById
  , hueTakeContextById
  ) where

import Data.List

-- -----------------------------------------------------------------------------
-- Contexts
-- | The 'HueContextManager' is used to store and manage all contexts of
-- the application
newtype HueContextManager context = HueContextManager
  { hueContexts :: [HueContext context]
  }

-- | The type of the context id
type ContextIdType = Int

-- | The 'HueContext' is a record created by the 'ContextManager' you
-- should never create an instence of it by yourself.
data (Eq context) =>
     HueContext context = HueContext
  { hueContextId :: ContextIdType
  , hueContext :: context
  } deriving (Eq, Show)

-- | Create a 'HueContextManager' instance with no contexts
hueCreateContextManager :: HueContextManager context
hueCreateContextManager = HueContextManager {hueContexts = []}

-- | Register a context in the 'HueContextManager' and return
-- a 'HueContext' instance
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

-- | Get a context in the 'HueContextManager' by 'hueContextId'
hueGetContextById ::
     Eq context
  => HueContextManager context
  -> ContextIdType
  -> Maybe (HueContext context)
hueGetContextById contextManager contextId =
  find (hueIsSameId contextId) (hueContexts contextManager)

-- | Get and remove a context in the 'HueContextManager' by 'hueContextId'
hueTakeContextById ::
     Eq context
  => HueContextManager context
  -> ContextIdType
  -> (HueContextManager context, Maybe (HueContext context))
hueTakeContextById contextManager contextId =
  case hueGetContextById contextManager contextId of
    Just context ->
      (contextManager {hueContexts = filteredContexts}, Just context)
      where filteredContexts =
              filter (not . hueIsSameId contextId) (hueContexts contextManager)
    Nothing -> (contextManager, Nothing)

-- | Check if the context is changed
hueHasContextChange ::
     Eq context
  => HueContextManager context
  -> ContextIdType
  -> HueContext context
  -> Either Bool Bool
hueHasContextChange contextManager contextId context =
  case hueGetContextById contextManager contextId of
    Just storedContext -> Right $ storedContext /= context
    Nothing -> Left True
