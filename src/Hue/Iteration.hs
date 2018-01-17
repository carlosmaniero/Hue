module Hue.Iteration where

-- |The `StateIteration` is a kind of `Iteration` monad that has as result a state
type StateIteration state = Iteration state state

-- |The `IterationUpdater` is a function defined to update a given state.
-- It takes two arguments the `msg` and the `state`.
-- Where `msg` is the message used to verify what kind of update should be performed.
--
-- @
--   data Msg = Increase | Decrease
--
--   type State = Int
--
--   updater :: IterationUpdater Msg  State
--   updater state msg = do
--     let result =
--       case msg of
--         Increase ->
--           state + 1
--         Decrease ->
--           state - 1
--     return result
-- @
--
-- The returned state will be passed as argument in the next update call.
type IterationUpdater msg state =
  state -> msg -> StateIteration state

-- |The `IterationTask` is an `IO` operation that returns a function that receive
-- an updated version of the application state and returns a new `StateIteration`.
--
-- It is created by the `process` function.
type IterationTask state = IO (state -> StateIteration state)

-- |`Iteration` is a type that orchestrates the state and the response is commonly used
-- as `StateIteration`.
-- It is a polyphoric type that has the `state` and `result` as flexible types.
data Iteration state result
  -- |Iteration has a list of `IterationTask`s and a result (commonly defined as `()`) when `>>=`
  -- to another `Iteration` a new `Iteration` is created with a join of their `IterationTask`s.
  = Iteration [IterationTask state] result
  | FinishedIteration state

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
-- process getLine $ \newState result -> return result
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
  -> (state -> result -> StateIteration state)
  -> Iteration state ()
process ioOperation callback =
  Iteration [task] ()
  where
    task = do
      result <- ioOperation
      return $ \state -> callback state result


-- | A version of process without callback, that is, it ignore the result of the `IO` operation
process_ :: IO result -> Iteration state ()
process_ ioOperation = process ioOperation $ \state _ -> return state


-- |This function is used to ignore any IO operations and context resolver
--
-- It's like an return of an imperative language that interrupt the execution. So, any bind (`>>=`)
-- performed after this will be ignored
finish :: state -> Iteration state state
finish = FinishedIteration

-- |The `fmap` function follows the monad `>>=` behavior.
instance Functor (Iteration state) where
  fmap f operation = do
    result <- operation
    return (f result)

instance Applicative (Iteration state) where
  pure = Iteration []
  Iteration givenTasks  functionResult <*> iteration =
    case fmap functionResult iteration of
      (Iteration newTasks  newIterationResult) ->
        Iteration joinedTasks  newIterationResult
        where
          joinedTasks = givenTasks ++ newTasks
      (FinishedIteration newState) ->
        FinishedIteration  newState
  FinishedIteration  state <*> _ = FinishedIteration state

-- |The main role of the `Iteration` monad is to keep the `IterationTask`s at each
-- bind (`>>=`).
--
-- This monad always gets the `IterationTask`s from the previous and current `Iteration`
-- and join both inside a new `Iteration` making a easy way to perform multiples tasks
-- using the `process` and return a new state using the `return` function.
instance Monad (Iteration state) where
  Iteration givenTasks result >>= f =
    case f result of
      (Iteration newTasks newResult) ->
        Iteration joinedTasks newResult
        where
          joinedTasks = givenTasks ++ newTasks
      (FinishedIteration state) ->
        FinishedIteration  state
  FinishedIteration state >>= _ = FinishedIteration state


-- |This is the result representation of an `StateIteration`. It contains basically the same
-- information of the `IterationTask`s it only adds the `husIterationStateResult` that contains
-- The final state of the `StateIteration`.
data IterationResult state =
  IterationResult { iterationStateResult :: state
                  , iterationTasksResult :: [IterationTask state]
                  }

-- |Convert a given `StateIteration` in a `IterationResult`
iterationToResult :: StateIteration state -> IterationResult state
iterationToResult (Iteration tasks state) =
  IterationResult state tasks
iterationToResult (FinishedIteration  state) =
  IterationResult state []

-- |The `performIteration` is used to call the `IterationUpdater` with the correct data.
-- It is a useful function to make your unit tests. You can see some examples in the
-- test/unit/IterationSpec.hs file.
performIteration :: state -> msg -> IterationUpdater msg state -> IterationResult state
performIteration state msg updater =
  iterationToResult $ updater state msg
