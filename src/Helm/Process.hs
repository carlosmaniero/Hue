module Helm.Process where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.DeepSeq


data ProcessType msg result
    = ProcessOnly (IO result) (result -> msg)
    | ProcessBulk (IO result) ([result] -> msg)
    | ProcessMany (IO result) (result -> msg)


data Process msg result =
    Process { processType :: ProcessType msg result
            , processBroadcast :: msg -> IO ()
            }


data CancellableProcess msg result =
    CancellableProcess { cancellableProcess :: Process msg result
                       , cancellableOperation :: IO Bool
                       , cancellableMsg :: msg
                       }


runOperationAsync :: MVar result -> IO result -> IO ThreadId
runOperationAsync channel operation = forkIO $ do
    result <- operation
    putMVar channel result


data Task msg = Task { taskId :: ThreadId
                     , taskBroadcast :: msg -> IO ()
                     }


runTask :: (msg -> IO ()) -> IO result -> (result -> msg) -> IO ()
runTask broadcast operation resultToMsg = do
    result <- operation
    broadcast $ resultToMsg result


processIOBulkParallel :: (msg -> IO ()) -> [IO result] -> ([result] -> msg) -> IO ()
processIOBulkParallel broadcast [] resultToMsg = do
    broadcast $ resultToMsg []
    return ()
processIOBulkParallel broadcast operations resultToMsg = do
    channel <- newEmptyMVar
    mapM_ (runOperationAsync channel) operations

    results <- replicateM (length operations) $ takeMVar channel
    broadcast $ resultToMsg results
    return ()


comunicateWithBroadcast :: Int -> MVar result -> (msg -> IO ()) -> (result -> msg) -> IO ()
comunicateWithBroadcast 0 _ _ _ = do
    return ()
comunicateWithBroadcast times channel broadcast resultToMsg = do
    result <- takeMVar channel
    broadcast $ resultToMsg result
    comunicateWithBroadcast (times - 1) channel broadcast resultToMsg


processIOParallel :: (msg -> IO ()) -> [IO result] -> (result -> msg) -> IO ()
processIOParallel broadcast operations resultToMsg = do
    channel <- newEmptyMVar
    mapM_ (runOperationAsync channel) operations
    forkIO $ comunicateWithBroadcast (length operations) channel broadcast resultToMsg
    return ()


startProcess :: Process msg result -> IO (Task msg)
startProcess process = do
    case processType process of
      ProcessOnly operation resultToMsg ->
        processOnly (processBroadcast process) operation resultToMsg


startCancellableProcess :: CancellableProcess msg result -> IO ()
startCancellableProcess process = do
    completedChannel <- newEmptyMVar
    let cancellableProcess' = (cancellableProcess process) { processBroadcast = proxyBroadcast completedChannel $ processBroadcast (cancellableProcess process) }
    let process' = process { cancellableProcess = cancellableProcess' }
    (runCancellableProcess
        (completedChannel)
        (cancellableMsg process')
        (cancellableOperation process')
        (startProcess $ cancellableProcess process'))
    return ()


processOnly :: (msg -> IO ()) -> IO result -> (result -> msg) -> IO (Task msg)
processOnly broadcast operation resultToMsg = do
    threadId <- forkIO $ runTask broadcast operation resultToMsg
    return $ Task { taskId = threadId, taskBroadcast = broadcast }

cancelTask :: msg -> Task msg -> IO ()
cancelTask msg task = do
    killThread $ taskId task
    (taskBroadcast task) msg

runCancelTask :: msg -> IO Bool -> Task msg -> IO ()
runCancelTask cancelMessage cancelTrigger task = do
    shouldCancel <- cancelTrigger
    if shouldCancel
       then
        cancelTask cancelMessage task
       else
        return ()


proxyBroadcast :: MVar Bool -> (msg -> IO ()) -> msg -> IO ()
proxyBroadcast channel broadcast msg = do
    putMVar channel True
    broadcast msg


runCancellableProcess :: MVar Bool -> msg -> IO Bool -> IO (Task msg) -> IO ThreadId
runCancellableProcess completedChannel cancelMessage cancelTrigger processExecutor = forkIO $ do
    process <- processExecutor
    cancelId <- forkIO $ runCancelTask cancelMessage cancelTrigger process
    completed <- takeMVar completedChannel
    killThread cancelId
    return ()
