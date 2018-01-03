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
type HueTaskIteration state response = state -> HueStateIteration state response

-- | This is the channel that tasks uses to communicate with its caller loop (`rueRunner`).
type HueIterationRunnerChannel state response = MVar (HueIterationRunnerMsgs state response)

-- | It contain the internal messages that tasks uses to communicate with
-- its caller loop (`hueRunner`).
data HueIterationRunnerMsgs state response
  -- | Used to start a new task.
  = HueStartTask (HueTaskIteration state response)
  -- | Used when the IO operation was successfully performed and a `HueStateIteration`
  -- is waiting for processing.
  | HueNewIteration ThreadId (HueTaskIteration state response)
  -- | Used when for some reason the IO operation is died.
  | HueTaskIterationDead ThreadId SomeException
  -- | Used by the `hueStop` to stop the loop execution.
  | HueStop


-- | The result of the `hueRun` loop. Each result return the last updated state.
data HueRunnerResult state
  -- | Yes! all tasks were performed and this is the final state
  = HueFinished state
  -- | You stopped the loop and this is the last version of the state
  | HueStopped state


-- | It starts a `HueIterationTask` in a new thread and respond for the `hueRun`
-- the result through the given channel.
hueStartTask
  :: HueIterationRunnerChannel state response
  -> HueIterationTask state response
  -> IO ThreadId
hueStartTask channel ioTask = forkIO $ do
    taskResult <- try ioTask
    threadId <- myThreadId
    case taskResult of
      Right task ->
        putMVar channel (HueNewIteration threadId task)
      Left e ->
        putMVar channel (HueTaskIterationDead threadId e)
    return ()


-- | It is the runner representation. Thought this you can stop
-- the execution using the `hueStop` and wait for the result state using
-- the `hueWait`.
--
-- Note: When the `hueStop` is called `hueRun` will kill all threads that
-- it started.
data HueRunner state = HueRunner { hueStop :: IO ()
                                 , hueWait :: IO (HueRunnerResult state) }

hueNextIteration
  :: state
  -> HueIterationRunnerChannel state response
  -> [ThreadId]
  -> IO (HueRunnerResult state)
hueNextIteration newState channel tasksRunning = do
  let newTasksRunning = tasksRunning
  if null newTasksRunning then
    return (HueFinished newState)
  else hueRun newTasksRunning channel newState


-- | The main loop that control the execution of of the `HueTaskIteration`.
-- It will perform the given task all its nested tasks and return an updated result.
hueRun :: [ThreadId] -> HueIterationRunnerChannel state response -> state -> IO (HueRunnerResult state)
hueRun tasksRunning channel state = do
  result <- takeMVar channel
  case result of
    (HueStartTask task) -> do
      let (HueIteration iterationData newState) = task state
      let tasks = hueIterationTasks iterationData
      newTasksId <- mapM (hueStartTask channel) tasks
      hueNextIteration newState channel (tasksRunning ++ newTasksId)
    (HueNewIteration threadId task) -> do
      let (HueIteration iterationData newState) = task state
      let tasks = hueIterationTasks iterationData
      newTasksId <- mapM (hueStartTask channel) tasks
      let newTasksRunning = filter (/= threadId) (tasksRunning ++ newTasksId)
      hueNextIteration newState channel newTasksRunning
    (HueTaskIterationDead threadId _) -> do
      let newTasksRunning = filter (/= threadId) tasksRunning
      hueNextIteration state channel newTasksRunning
    HueStop -> do
      mapM_ killThread tasksRunning
      return (HueStopped state)


-- | Start the iteration and return a `HueRunner` where is possible to control the
-- execution and get its result.
hueStartIteration :: (HueTaskIteration state response) -> state -> IO (HueRunner state)
hueStartIteration task state = do
  channel <- newMVar (HueStartTask task)
  channelState <- newEmptyMVar
  _ <- forkIO $ do
    result <- hueRun [] channel state
    putMVar channelState result

  return (HueRunner
           (putMVar channel HueStop)
           (readMVar channelState))

