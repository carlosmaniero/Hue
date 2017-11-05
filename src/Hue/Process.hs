module Hue.Process
    ( ProcessType(..)
    , Process(..)
    , CancellableProcess(..)
    , Task
    , startProcess
    , startCancellableProcess
    )
where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

-- -----------------------------------------------------------------------------
-- Process


-- | The process type where:
--
-- * 'ProcessOnly' represents a process with just one operation to be executed.
--
-- * 'ProcessBulk' executes many operations but join all of their results in
-- one message
--
-- * 'ProcessMany' is like 'ProcessBulk' but for all operation completed a
-- message with it result is sent.
--
-- The first argument of all these types is the operation to be processed
-- followed by a function to transform the result in a message
data ProcessType msg result
    = ProcessOnly (IO result) (result -> msg)
    | ProcessBulk [IO result] ([result] -> msg)
    | ProcessMany [IO result] (result -> msg)


-- | 'Process' is an abstract representation of a process. It contains the
-- type of the process and the broadcast where the result message will
-- be send when the operation is done.
data Process msg result =
    Process { processType :: ProcessType msg result
            , processBroadcast :: msg -> IO ()
            }

processOperationLength :: Process msg result -> Int
processOperationLength process =
    case processType process of
      ProcessOnly operation createMsg ->
          1
      ProcessBulk operations createMsg ->
          1
      ProcessMany operations createMsg ->
          length operations

-- -----------------------------------------------------------------------------
-- Running Process

-- | The task contains information about the process execution like the
type Task = ThreadId

-- | Given a 'Process' the 'startProcess' will running it and return a 'Task'
startProcess :: Process msg result -> IO Task
startProcess process = do
    let broadcast = processBroadcast process
    case processType process of
      ProcessOnly operation createMsg ->
          startProcessOnly operation createMsg broadcast
      ProcessBulk operations createMsg ->
          startProcessBulk operations createMsg broadcast
      ProcessMany operations createMsg ->
          startProcessMany operations createMsg broadcast


startProcessOnly :: IO result -> (result -> msg) -> (msg -> IO ()) -> IO Task
startProcessOnly operation createMsg broadcast = forkIO $ do
    result <- operation
    broadcast $ createMsg result


startProcessMany :: [IO result] -> (result -> msg) -> (msg -> IO ()) -> IO Task
startProcessMany operations createMsg broadcast = forkIO $ do
    mapM_ (\operation -> startProcessOnly operation createMsg broadcast) operations


startProcessBulk :: [IO result] -> ([result] -> msg) -> (msg -> IO ()) -> IO Task
startProcessBulk operations createMsg broadcast = forkIO $ do
    channel <- newEmptyMVar
    mapM_ (\operation -> runOperationAsync operation $ putMVar channel) operations

    results <- replicateM (length operations) $ takeMVar channel
    broadcast $ createMsg results


runOperationAsync :: IO result -> (result -> IO ()) -> IO Task
runOperationAsync operation broadcast = forkIO $ do
    result <- operation
    broadcast result



-- -----------------------------------------------------------------------------
-- Running Cancellable Process


-- | The cancellable process representation where:
--
-- * 'cancellableProcess' represents a 'Process' that can be cancelled
--
-- * 'cancellableOperation' is an 'IO' operation that returns a 'Bool'. If it
-- returns 'True'. The 'cancellableProcess' is killed.
--
-- * 'cancellableMsg' the message sent when the process is cancelled
--
-- If the process is completed before the cancellable operation. It does not send
-- the cancellable msg.
data CancellableProcess msg result =
    CancellableProcess { cancellableProcess :: Process msg result
                       , cancellableOperation :: IO Bool
                       , cancellableMsg :: msg
                       }


data CancellableMsgType msg = CancellableMsg | CancellableProcessMsg msg


-- | Start a process that can be canceled by a given 'cancellableOperation' function
startCancellableProcess :: CancellableProcess msg result -> IO Task
startCancellableProcess cancellable = forkIO $ do
    blockedChannel <- newEmptyMVar

    let blockedBroadcast = putMVar blockedChannel
    let process = cancellableProcess cancellable
    let originalBroadcast = processBroadcast process
    let processWithBlockedBroadcast = process { processBroadcast = blockedBroadcast . CancellableProcessMsg}

    processTask <- startProcess processWithBlockedBroadcast
    cancellableTask <- startCancellableOperation (cancellableOperation cancellable) (cancellableMsg cancellable) blockedBroadcast

    manageCancellableTasks (processOperationLength processWithBlockedBroadcast) blockedChannel (cancellableMsg cancellable) originalBroadcast processTask cancellableTask


manageCancellableTasks :: Int -> MVar (CancellableMsgType msg) -> msg -> (msg -> IO()) -> Task -> Task -> IO ()
manageCancellableTasks 0 _ _ _ processTask cancellableTask = do
    killThread processTask
    killThread cancellableTask
manageCancellableTasks expectedCalls blockedChannel cancelledMsg broadcast processTask cancellableTask = do
    msg <- takeMVar blockedChannel

    case msg of
      CancellableMsg ->
          cancelProcess cancelledMsg broadcast processTask cancellableTask
      CancellableProcessMsg msg ->
          (do
              broadcast msg
              manageCancellableTasks (expectedCalls - 1) blockedChannel cancelledMsg broadcast processTask cancellableTask
          )


startCancellableOperation :: (IO Bool) -> msg -> (CancellableMsgType msg -> IO ()) -> IO Task
startCancellableOperation operation msg broadcast = forkIO $ do
    result <- operation
    if result
       then broadcast CancellableMsg
    else return ()


cancelProcess :: msg -> (msg -> IO()) -> Task -> Task -> IO ()
cancelProcess cancelledMsg broadcast processTask cancellableTask = do
    killThread processTask
    killThread cancellableTask
    broadcast cancelledMsg
