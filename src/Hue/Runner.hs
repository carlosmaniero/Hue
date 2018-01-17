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
type TaskIteration state = state -> StateIteration state

-- | This is the channel that tasks uses to communicate with its caller loop (`rueRunner`).
type IterationRunnerChannel state = MVar (IterationRunnerMsgs state)

-- | It contain the internal messages that tasks uses to communicate with
-- its caller loop (`runner`).
data IterationRunnerMsgs state
  -- | Used to start a new task.
  = StartTask (TaskIteration state)
  -- | Used when the IO operation was successfully performed and a `StateIteration`
  -- is waiting for processing.
  | NewIteration ThreadId (TaskIteration state)
  -- | Used when for some reason the IO operation is died.
  | TaskIterationDead ThreadId SomeException
  -- | Used by the `hueStop` to stop the loop execution.
  | StopRunner


-- | The finished state of the `run` loop.
data RunnerFinishedStatus
  -- | Yes! all tasks were performed and this is the final state
  = Completed
  -- | An operation finished the execution by the `finish` calling
  | Finished
  -- | You stopped the loop and this is the last version of the state
  | Stopped
  deriving (Eq, Show)


-- | The runner response contains the last updated state and why the
-- runner finished its operation
type RunnerResult state = (state, RunnerFinishedStatus)


-- | It starts a `IterationTask` in a new thread and respond for the `run`
-- the result through the given channel.
startTask
  :: IterationRunnerChannel state
  -> IterationTask state
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
data Runner state  = Runner { stop :: IO ()
                            , schedule :: TaskIteration state  -> IO ()
                            , wait :: IO (RunnerResult state) }

nextRunIteration
  :: Bool
  -> state
  -> IterationRunnerChannel state
  -> [ThreadId]
  -> IO (RunnerResult state)
nextRunIteration runForever newState channel tasksRunning = do
  let newTasksRunning = tasksRunning
  if null newTasksRunning && not runForever then
    return (newState, Completed)
  else run runForever newTasksRunning channel newState


processIteration
  :: Bool
  -> [ThreadId]
  -> IterationRunnerChannel state
  -> StateIteration state
  -> IO (RunnerResult state)
processIteration runForever tasksRunning channel iteration =
  case iteration of
    Iteration tasks newState -> do
      newTasksId <- mapM (startTask channel) tasks
      nextRunIteration runForever newState channel (tasksRunning ++ newTasksId)
    FinishedIteration newState ->
      return (newState, Finished)


-- | The main loop that control the execution of of the `TaskIteration`.
-- It will perform the given task all its nested tasks and return an updated result.
run :: Bool -> [ThreadId] -> IterationRunnerChannel state -> state -> IO (RunnerResult state)
run runForever tasksRunning channel state = do
  result <- takeMVar channel
  case result of
    (StartTask task) ->
      processIteration runForever tasksRunning channel (task state)
    (NewIteration originThreadId task) ->
      processIteration runForever newTasksRunning channel (task state)
          where newTasksRunning = filter (/= originThreadId) tasksRunning
    (TaskIterationDead threadId _) -> do
      let newTasksRunning = filter (/= threadId) tasksRunning
      nextRunIteration runForever state channel newTasksRunning
    StopRunner -> do
      mapM_ killThread tasksRunning
      return (state, Stopped)


startRunner' :: Bool -> state -> IO (Runner state)
startRunner' runForever initialState = do
  channel <- newEmptyMVar
  channelState <- newEmptyMVar
  _ <- forkIO $ do
    result <- run runForever [] channel initialState
    putMVar channelState result

  return (Runner
           (putMVar channel StopRunner)
           (putMVar channel . StartTask)
           (readMVar channelState))


startRunner :: state -> IO (Runner state)
startRunner = startRunner' False

startForeverRunner :: state -> IO (Runner state)
startForeverRunner = startRunner' True
