module Hue.Runner where

import Hue.Iteration
import Control.Concurrent
import Control.Exception


-- | The result of an IO operation.
--
-- When a IO operation is completed it return always a function that receives
-- a state as argument and return a new state iteration.
--
-- This mechanism is created by the `huePerformTask`.
type TaskIteration state response = state -> StateIteration state response

-- | This is the channel that tasks uses to communicate with its caller loop (`rueRunner`).
type IterationRunnerChannel state response = MVar (IterationRunnerMsgs state response)

-- | It contain the internal messages that tasks uses to communicate with
-- its caller loop (`runner`).
data IterationRunnerMsgs state response
  -- | Used to start a new task.
  = StartTask (TaskIteration state response)
  -- | Used when the IO operation was successfully performed and a `StateIteration`
  -- is waiting for processing.
  | NewIteration ThreadId (TaskIteration state response)
  -- | Used when for some reason the IO operation is died.
  | TaskIterationDead ThreadId SomeException
  -- | Used by the `hueStop` to stop the loop execution.
  | StopRunner


-- | The result of the `run` loop. Each result return the last updated state.
data RunnerResult state
  -- | Yes! all tasks were performed and this is the final state
  = Finished state
  -- | You stopped the loop and this is the last version of the state
  | Stopped state


-- | It starts a `IterationTask` in a new thread and respond for the `run`
-- the result through the given channel.
startTask
  :: IterationRunnerChannel state response
  -> IterationTask state response
  -> IO ThreadId
startTask channel ioTask = forkIO $ do
    taskResult <- try ioTask
    threadId <- myThreadId
    case taskResult of
      Right task ->
        putMVar channel (NewIteration threadId task)
      Left e ->
        putMVar channel (TaskIterationDead threadId e)
    return ()


-- | It is the runner representation. Thought this you can stop
-- the execution using the `stop` and wait for the result state using
-- the `wait`.
--
-- Note: When the `stop` is called `run` will kill all threads that
-- it started.
data Runner state = Runner { stop :: IO ()
                           , wait :: IO (RunnerResult state) }

nextRunIteration
  :: state
  -> IterationRunnerChannel state response
  -> [ThreadId]
  -> IO (RunnerResult state)
nextRunIteration newState channel tasksRunning = do
  let newTasksRunning = tasksRunning
  if null newTasksRunning then
    return (Finished newState)
  else run newTasksRunning channel newState


processIteration
  ::[ThreadId]
  -> IterationRunnerChannel state response
  -> StateIteration state response
  -> IO (RunnerResult state)
processIteration tasksRunning channel iteration =
  case iteration of
    Iteration tasks _ newState -> do
      newTasksId <- mapM (startTask channel) tasks
      nextRunIteration newState channel (tasksRunning ++ newTasksId)
    FinishedIteration _ newState ->
      return (Finished newState)


-- | The main loop that control the execution of of the `TaskIteration`.
-- It will perform the given task all its nested tasks and return an updated result.
run :: [ThreadId] -> IterationRunnerChannel state response -> state -> IO (RunnerResult state)
run tasksRunning channel state = do
  result <- takeMVar channel
  case result of
    (StartTask task) ->
      processIteration tasksRunning channel (task state)
    (NewIteration originThreadId task) ->
      processIteration newTasksRunning channel (task state)
          where newTasksRunning = filter (/= originThreadId) tasksRunning
    (TaskIterationDead threadId _) -> do
      let newTasksRunning = filter (/= threadId) tasksRunning
      nextRunIteration state channel newTasksRunning
    StopRunner -> do
      mapM_ killThread tasksRunning
      return (Stopped state)


-- | Start the iteration and return a `Runner` where is possible to control the
-- execution and get its result.
startIteration :: (TaskIteration state response) -> state -> IO (Runner state)
startIteration task state = do
  channel <- newMVar (StartTask task)
  channelState <- newEmptyMVar
  _ <- forkIO $ do
    result <- run [] channel state
    putMVar channelState result

  return (Runner
           (putMVar channel StopRunner)
           (readMVar channelState))
