module Helm.Process
    ( processIO
    , processIOParallel
    , processIOBulkParallel
    ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.DeepSeq


runOperationAsync :: MVar result -> IO result -> IO ThreadId
runOperationAsync channel operation = forkIO $ do
    result <- operation
    putMVar channel result


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


processIO :: (msg -> IO ()) -> IO result -> (result -> msg) -> IO ()
processIO broadcast operation resultToMsg = do
    channel <- newEmptyMVar
    runOperationAsync channel operation
    result <- takeMVar channel
    broadcast $ resultToMsg result
    return ()
