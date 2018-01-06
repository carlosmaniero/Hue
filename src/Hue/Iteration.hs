module Hue.Iteration where

import Hue.Context

-- |The `StateIteration` is a kind of `Iteration` monad that has as result a state
type StateIteration state response = Iteration state response state

-- |The `Resolver` type is used to represent the `respond` with a `Context`.
--
-- This is the first argument of a `IterationUpdater` and is used to respond
-- to the `dispatch` caller.
type Resolver state response = response -> Iteration state response ()

-- |The `IterationUpdater` is a function defined to update a given state.
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
--   updater :: IterationUpdater Msg Response State
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
-- `resolver` acts like a Javascript Promise, that is, it will send the given `Response`
-- to the update caller. You should see how it works in the `dispatch` documentation.
type IterationUpdater msg response state =
  Resolver state response -> state -> msg -> StateIteration state response

-- |The `IterationTask` is an `IO` operation that returns a function that receive
-- an updated version of the application state and returns a new `StateIteration`.
--
-- It is created by the `process`.
--
-- This is used to inside the `IterationData` in the `Iteration` to represent
-- An `IO` operation that should be performed and when it was completely performed
-- it should perform a new iteration.
type IterationTask state response = IO (state -> StateIteration state response)

-- |This type contains a context and it given reponse.
--
-- As the `IterationTask` this is also used inside the `IterationData` in the
-- `Iteration`.
type IterationResponse response = (Context, response)

-- |`Iteration` is a type that orchestrates the state and the response is commonly used
-- as `StateIteration`.
-- It is a polyphoric type that has the `state`, `response` and `result` as flexible types.
data Iteration state response result
  -- |Iteration has a `IterationData` and a result (commonly defined as `()`) when `>>=`
  -- to another `Iteration` a new `Iteration` is created with a join of their `IterationData`.
  = Iteration [IterationTask state response] [IterationResponse response] result
  | FinishedIteration [IterationResponse response] state

-- |Register an `IO` operation to performed. It receives the `IO` operation and a callback function that
-- returns a `StateIteration`.
--
-- The callback function must receive the `IO` operation result and the updated state of the application.
--
-- The state is provided by the `` loop (TODO: link the  loop here). It is necessary because the
-- `IO` operation will run in parallel and is possible that the state provided in the `updater` function
-- was outdated.
--
-- Simple example:
--
-- @
-- process getLine \result newState -> return result
-- @
--
-- In this example, the state is a `String` and when the `getLine` is completed, the callback function will
-- be called with the inserted text in the `stdin` as `result` and the callback return the given `result`.
-- That is, the next iteration in the `updater` function will be performed with the text provided by the
-- `getLine`.
--
-- The `IO` operation will be performed in parallel and the callback concurrently.
process
  :: IO result
  -> (state -> result -> StateIteration state response)
  -> Iteration state response ()
process ioOperation callback =
  Iteration [task] [] ()
  where
    task = do
      result <- ioOperation
      return $ \state -> callback state result


-- | A version of process without callback, that is, it ignore the result of the `IO` operation
process_ :: IO result -> Iteration state response ()
process_ ioOperation = process ioOperation $ \state _ -> return state


-- |This function is used to generate a `Resolver` given a `Context`.
respond :: Context -> Resolver state response
respond context response =
  Iteration [] [(context, response)] ()

-- |This function is used to ignore any IO operations and context resolver
--
-- It's like an return of an imperative language that interrupt the execution. So, any bind (`>>=`)
-- performed after this will be ignored
finish :: state -> Iteration state response state
finish = FinishedIteration []


-- |The `fmap` function follows the monad `>>=` behavior.
instance Functor (Iteration state response) where
  fmap f operation = do
    result <- operation
    return (f result)

instance Applicative (Iteration state response) where
  pure = Iteration [] []
  Iteration givenTasks givenResponses functionResult <*> iteration = joinedIteration
    where
      (Iteration newTasks newResponses newIterationResult) = fmap functionResult iteration
      joinedTasks = givenTasks ++ newTasks
      joinedResponses = givenResponses ++ newResponses
      joinedIteration = Iteration joinedTasks joinedResponses newIterationResult


-- |The main role of the `Iteration` monad is to keep the `IterationData` at each
-- bind (`>>=`).
--
-- This monad always gets the `IterationData` from the previous and current `Iteration`
-- and join both inside a new `Iteration` making a easy way to perform multiples tasks
-- using the `process`, resolve many requests using the `Resolver` and return a
-- new state using the `return` function.
instance Monad (Iteration state response) where
  Iteration givenTasks givenResponses result >>= f =
    case f result of
      (Iteration newTasks newResponses newResult) ->
        Iteration joinedTasks joinedResponses newResult
        where
          joinedTasks = givenTasks ++ newTasks
          joinedResponses = givenResponses ++ newResponses
      (FinishedIteration newResponses state) ->
        FinishedIteration (givenResponses ++ newResponses) state
  FinishedIteration responses state >>= _ = FinishedIteration responses state


-- |This is the result representation of an `StateIteration`. It contains basically the same
-- information of the `IterationData` it only adds the `husIterationStateResult` that contains
-- The final state of the `StateIteration`.
data IterationResult state response =
  IterationResult { iterationStateResult :: state
                  , iterationTasksResult :: [IterationTask state response]
                  , iterationResponsesResult :: [IterationResponse response]}

-- |Convert a given `StateIteration` in a `IterationResult`
iterationToResult :: StateIteration state response -> IterationResult state response
iterationToResult (Iteration tasks responses state) =
  IterationResult state tasks responses
iterationToResult (FinishedIteration responses state) =
  IterationResult state [] responses

-- |The `performIteration` is used to call the `IterationUpdater` with the correct data.
-- It is a useful function to make your unit tests. You can see some examples in the
-- test/unit/IterationSpec.hs file.
performIteration :: Context -> state -> msg -> IterationUpdater msg response state -> IterationResult state response
performIteration context state msg updater =
  iterationToResult $ updater (respond context) state msg
