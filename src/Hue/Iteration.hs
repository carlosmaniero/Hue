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

-- |The `HueIterationTask` encapsulates inside the `IO` Monad a function that receive
-- an updated version of the application state and returns a new HueStateIterataion.
-- It is created by the `huePerformTask`.
type HueIterationTask state response = IO (state -> HueStateIteration state response)

-- |This type contains a context and the context response
type HueIterationResponse response = (HueContext, response)

-- |The `HueIterationData` contains information that should be passed for each monad
-- binding.
data HueIterationData state response = HueIterationData
            { hueIterationTasks :: [HueIterationTask state response]
            , hueIterationResponses :: [HueIterationResponse response]
            }

-- |This function receive two `HueIterationData` and create a new joining their data.
hueJoinIterationData :: HueIterationData state response -> HueIterationData state response -> HueIterationData state response
hueJoinIterationData iterationA iterationB = HueIterationData tasks responses
  where
    tasks = (hueIterationTasks iterationA) ++ (hueIterationTasks iterationB)
    responses = (hueIterationResponses iterationA) ++ (hueIterationResponses iterationB)

-- |`HueIteration` is a type that orchestrates the state and the response is commonly used
-- as `HueStateIteration`.
-- It is a polyphoric type that has the `state`, `response` and `result` as flexible types.
data HueIteration state response result
  -- |HueIteration has a `HueIterationData` and a result (commonly defined as `()`) when `>>=`
  -- to another `HueIteration` a new `HueIteration` is created with a join of their `HueIterationData`.
  = HueIteration { hueIterationData :: (HueIterationData state response)
                 , hueIterationResult :: result
                 }
  -- |It's used in `return` at the `Applicative` instance.


-- |Register an `IO` operation to performed. It receives the `IO` operation and a callback function that
-- returns a `HueStateIteration`.
--
-- The callback function must receive the `IO` operation result and the updated state of the application.
--
-- The state is provided by the `Hue` loop (TODO: link the Hue loop here). It is necessary because the
-- `IO` operation will run in parallel and is possible that the state provided in the `updater` function
-- was outdated.
--
-- Simple example:
--
-- @
-- huePerformTask getLine \result newState -> return result
-- @
--
-- In this example, the state is a `String` and when the `getLine` is completed, the callback function will
-- be called with the inserted text in the `stdin` as `result` and the callback return the given `result`.
-- That is, the next iteration in the `updater` function will be performed with the text provided by the
-- `getLine`.
--
-- The `IO` operation will be performed in parallel and the callback concurrently.
huePerformTask
  :: IO result
  -> (state -> result -> HueStateIteration state response)
  -> HueIteration state response ()
huePerformTask ioOperation callback =
  HueIteration hueIterationData ()
  where
    task = do
      result <- ioOperation
      return $ \state -> callback state result
    hueIterationData = HueIterationData [task] []

-- |This function is used to generate a `HueResolver` given a `HueContext`.
hueRespond :: HueContext -> HueResolver state response
hueRespond context response =
  HueIteration (HueIterationData [] [(context, response)]) ()

-- |The `fmap` function follows the monad `>>=` behavior.
instance Functor (HueIteration state response) where
  fmap f operation = do
    result <- operation
    return (f result)


instance Applicative (HueIteration state response) where
  pure = HueIteration (HueIterationData [] [])
  HueIteration givenHueIterationData functionResult <*> iteration = hueIteration
    where
      newHueIteration = fmap functionResult iteration
      newHueIterationData = hueIterationData newHueIteration
      newHueIterationResult = hueIterationResult newHueIteration
      joinedHueIterationData = hueJoinIterationData givenHueIterationData newHueIterationData
      hueIteration = HueIteration joinedHueIterationData newHueIterationResult


-- |The main role of the `HueIteration` monad is to keep the `HueIterationData` at each
-- bind (`>>=`).
--
-- This monad always gets the `HueIterationData` from the previous and current `HueIteration`
-- and join both inside a new `HueIteration` making a easy way to perform multiples tasks
-- using the `huePerformTask`, resolve many requests using the `HueResolver` and return a
-- new state using the `HueResult` or the `return` function.
instance Monad (HueIteration state response) where
  HueIteration iterationData result >>= f =
    case f result of
      HueIteration newIterationData newResult ->
        HueIteration (hueJoinIterationData iterationData newIterationData) newResult


-- |This is the result representation of an `HueStateIteration`. It contains basically the same
-- information of the `HueIterationData` it only adds the `husIterationStateResult` that contains
-- The final state of the `HueStateIteration`.
data HueIterationResult state response = HueIterationResult { hueIterationStateResult :: state
                                                            , hueIterationTasksResult :: [HueIterationTask state response]
                                                            , hueIterationResponsesResult :: [HueIterationResponse response]}

-- |Convert a given `HueStateIteration` in a `HueIterationResult`
hueIterationToResult :: HueStateIteration state response -> HueIterationResult state response
hueIterationToResult (HueIteration iterationData state) = HueIterationResult state (hueIterationTasks iterationData) (hueIterationResponses iterationData)

-- |The `huePerformIteration` is used to call the `HueIterationUpdater` with the correct data.
-- It is a useful function to make your unit tests. You can see some examples in the
-- test/unit/HueIterationSpec.hs file.
huePerformIteration :: HueContext -> state -> msg -> HueIterationUpdater msg response state -> HueIterationResult state response
huePerformIteration context state msg updater =
  hueIterationToResult $ updater (hueRespond context) state msg
