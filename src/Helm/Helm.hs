module Helm
    ( processIO
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
processIOBulkParallel update model resultMsg operations = do
    channel <- newEmptyMVar
    mapM_ (runOperationAsync channel) operations

    results <- replicateM (length operations) $ takeMVar channel
    return $ update (resultMsg results) model


processIO :: (msg -> model -> model) -> model -> IO msg -> IO model
processIO update model operation = do
    channel <- newEmptyMVar
    runOperationAsync channel operation
    msg' <- takeMVar channel
    return $ update msg' model
