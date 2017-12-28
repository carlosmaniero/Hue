module Hue.Iteration where

import Hue.Context

-- |The `HueStateIteration` is a kind of `HueIteration` monad that has as result a state
type HueStateIteration state response = HueIteration state response state


-- |The `HueResolver` type is used to represent the `hueRespond` with a `HueContext`.
-- This is the first argument of an `HueIterationUpdater` and is used to respond
-- to the `hueDispath` caller.
type HueResolver state response = response -> HueIteration state response ()

-- |The `HueIterationUpdater` is a function defined to update a given state.
-- It takes three arguments the `resolver`, the `msg` and the `state`.
-- Where `msg` is the message used to verify what kind of update should be performed.
--
-- @
--   data Msg = Increase | Decrease
--
--   type Response = Result Int
--
--   type State = Int
--
--   updater :: HueIterationUpdater Msg Response State
--   updater resolver state msg = do
--     let result =
--       case msg of
--         Increase ->
--           state + 1
--         Decrease ->
--           state - 1
--     resolver result
--     return result
-- @
--
-- The returned state will be passed as argument in the next update call and the
-- `resolver` acts like a Javascript Promise that is it will sent the given `Response`
-- to the update caller. You should see how it works in the `hueDispatch` documentation.
type HueIterationUpdater msg response state =
  HueResolver state response -> state -> msg -> HueStateIteration state response

type HueIterationTask state response = state -> IO (HueStateIteration state response)

type HueIterationResponse response = (HueContext, response)

data HueIterationData state response = HueIterationData
            { hueIterationTasks :: [HueIterationTask state response]
            , hueIterationResponses :: [HueIterationResponse response]
            }

emptyIterationData = HueIterationData [] []

hueJoinIterationData :: HueIterationData state response -> HueIterationData state response -> HueIterationData state response
hueJoinIterationData iterationA iterationB = HueIterationData tasks responses
  where
    tasks = (hueIterationTasks iterationA) ++ (hueIterationTasks iterationB)
    responses = (hueIterationResponses iterationA) ++ (hueIterationResponses iterationB)


hueGetIterationData :: HueIteration state response result -> HueIterationData state response
hueGetIterationData (HueOperation iterationData _) = iterationData
hueGetIterationData _ = emptyIterationData

hueGetIterationResult :: HueIteration state response result -> result
hueGetIterationResult (HueResult result) = result
hueGetIterationResult (HueOperation _ result) = result


data HueIteration state response result
  = HueOperation (HueIterationData state response) result
  | HueResult result


huePerformTask
  :: IO result
  -> (state -> result -> HueStateIteration state response)
  -> HueIteration state response ()
huePerformTask ioOperation callback =
  HueOperation hueIterationData ()
  where
    task state = do
      result <- ioOperation
      return $ callback state result
    hueIterationData = HueIterationData [task] []

hueRespond :: HueContext -> response -> HueIteration state response ()
hueRespond context response =
  HueOperation (HueIterationData [] [(context, response)]) ()

instance Functor (HueIteration state response) where
  fmap f operation = do
    result <- operation
    return (f result)


instance Applicative (HueIteration state response) where
  pure = HueResult
  HueResult functionResult <*> iteration = fmap functionResult iteration
  HueOperation hueIterationData functionResult <*> iteration = hueIteration
    where
      newHueIteration = fmap functionResult iteration
      newHueIterationData = hueGetIterationData newHueIteration
      newHueIterationResult = hueGetIterationResult newHueIteration
      joinedHueIterationData = hueJoinIterationData hueIterationData newHueIterationData
      hueIteration = HueOperation joinedHueIterationData newHueIterationResult


instance Monad (HueIteration state response) where
  HueResult result >>= f = f result
  HueOperation iterationData result >>= f =
    case f result of
      HueResult newResult ->
        HueOperation iterationData newResult
      HueOperation newIterationData newResult ->
        HueOperation (hueJoinIterationData iterationData newIterationData) newResult


data HueIterationResult state response = HueIterationResult { hueIterationStateResult :: state
                                                            , hueIterationTasksResult :: [HueIterationTask state response]
                                                            , hueIterationResponsesResult :: [HueIterationResponse response]}


hueIterationToResult :: HueStateIteration state response -> HueIterationResult state response
hueIterationToResult (HueResult state) = HueIterationResult state [] []
hueIterationToResult (HueOperation iterationData state) = HueIterationResult state (hueIterationTasks iterationData) (hueIterationResponses iterationData)

huePerformIteration :: HueContext -> state -> msg -> HueIterationUpdater msg response state -> HueIterationResult state response
huePerformIteration context state msg updater =
  hueIterationToResult $ updater (hueRespond context) state msg
