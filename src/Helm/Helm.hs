module Helm
    ( processIO
    , processIOParallel
    , processIOBulkParallel
    ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad


runOperationAsync :: MVar result -> IO result -> IO ThreadId
runOperationAsync channel operation = forkIO $ do
    result <- operation
    putMVar channel result


processIOBulkParallel :: (msg -> model -> model) -> model -> ([result] -> msg) -> [IO result] -> IO model
processIOBulkParallel _ model _ [] = do
    return model
processIOBulkParallel update model resultMsg operations = do
    channel <- newEmptyMVar
    mapM_ (runOperationAsync channel) operations

    results <- replicateM (length operations) $ takeMVar channel
    return $ update (resultMsg results) model


updateParallelModel :: Int -> MVar result -> (msg -> model -> model) -> model -> (result -> msg) -> IO model
updateParallelModel 0 _ _ model _ = do
    return model
updateParallelModel times channel update model resultMsg = do
    result <- takeMVar channel
    let updatedModel = update (resultMsg result) model
    updateParallelModel (times - 1) channel update updatedModel resultMsg


processIOParallel :: (msg -> model -> model) -> model -> (result -> msg) -> [IO result] -> IO model
processIOParallel _ model _ [] = do
    return model
processIOParallel update model resultMsg operations = do
    channel <- newEmptyMVar
    mapM_ (runOperationAsync channel) operations
    updateParallelModel (length operations) channel update model resultMsg


processIO :: (msg -> model -> model) -> model -> IO msg -> IO model
processIO update model operation = do
    channel <- newEmptyMVar
    runOperationAsync channel operation
    msg' <- takeMVar channel
    return $ update msg' model
