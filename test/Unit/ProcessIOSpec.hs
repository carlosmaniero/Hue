module ProcessIOSpec where

import Hue.Process
import Test.Hspec
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception.Base


processIOSpec :: SpecWith ()
processIOSpec = do
    describe "Given an IO Operation and an update function" $ do
        it "should call the update with the IO result message" $ do
            channel <- newEmptyMVar
            startProcess $ Process { processType = ProcessOnly theAnswer Completed
                                   , processBroadcast = channelBroadcast channel
                                   , processCancellable = Nothing }
            msg' <- takeMVar channel
            msg' `shouldBe` (Completed 42)
        describe "Cancelling the process" $ do
            it "should cancel the process given an cancel function" $ do
                channel <- newEmptyMVar
                startProcess $ Process { processType = ProcessOnly theSlowAnswer Completed
                                       , processBroadcast = channelBroadcast channel
                                       , processCancellable =
                                            Just Cancellable { cancellableMsg = Canceled
                                                             , cancellableOperation = cancelImmediately }
                                       }
                msg' <- takeMVar channel
                msg' `shouldBe` (Canceled)
                takeMVar channel `shouldThrow` anyException
            it "should execute the process if the cancel function returns false" $ do
                channel <- newEmptyMVar
                startProcess $ Process { processType = ProcessOnly theSlowAnswer Completed
                                       , processBroadcast = channelBroadcast channel
                                       , processCancellable =
                                            Just Cancellable { cancellableMsg = Canceled
                                                             , cancellableOperation = neverCancel }
                                       }
                msg' <- takeMVar channel
                msg' `shouldBe` (Completed 42)
                takeMVar channel `shouldThrow` anyException
            it "shouldn't send the canceled message if the process already completed" $ do
                channel <- newEmptyMVar
                startProcess $ Process { processType = ProcessOnly theAnswer Completed
                                       , processBroadcast = channelBroadcast channel
                                       , processCancellable =
                                            Just Cancellable { cancellableMsg = Canceled
                                                             , cancellableOperation = theSlowCancel }
                                       }
                msg' <- takeMVar channel
                msg' `shouldBe` (Completed 42)
                takeMVar channel `shouldThrow` anyException

    describe "Given a list of IO Operation and an update function" $ do
        describe "When I expect the parallel operation to update the full list of result" $ do
            it "should process all the list of IO operation" $ do
                channel <- newEmptyMVar
                startProcess $ Process { processType = ProcessBulk [theAnswer, theNumber, theAnotherNumber] ProcessedList
                                       , processBroadcast = channelBroadcast channel
                                       , processCancellable = Nothing }
                msg' <- takeMVar channel
                shouldBeEqualProcessedList [1993, 42, 7] msg'
            it "should do nothing given a empty list" $ do
                channel <- newEmptyMVar
                startProcess $ Process { processType = ProcessBulk [] ProcessedList
                                       , processBroadcast = channelBroadcast channel
                                       , processCancellable = Nothing }
                msg' <- takeMVar channel
                shouldBeEqualProcessedList [] msg'
            describe "Cancelling the process" $ do
                it "should cancel the process given a cancellable function" $ do
                    channel <- newEmptyMVar
                    startProcess $ Process { processType = ProcessBulk [theSlowAnswer, theSlowAnswer] ProcessedList
                                           , processBroadcast = channelBroadcast channel
                                           , processCancellable =
                                                Just Cancellable { cancellableMsg = Canceled
                                                                 , cancellableOperation = cancelImmediately }
                                           }
                    msg' <- takeMVar channel
                    msg' `shouldBe` (Canceled)
                    takeMVar channel `shouldThrow` anyException
                it "shouldn't send the canceled message if the process already completed" $ do
                    channel <- newEmptyMVar
                    startProcess $ Process { processType = ProcessBulk [theAnswer, theAnswer] ProcessedList
                                           , processBroadcast = channelBroadcast channel
                                           , processCancellable =
                                                Just Cancellable { cancellableMsg = Canceled
                                                                 , cancellableOperation = theSlowCancel }
                                           }
                    msg' <- takeMVar channel
                    shouldBeEqualProcessedList [42, 42] msg'
                    takeMVar channel `shouldThrow` anyException
        describe "When I expect the parallel operation to update each operation" $ do
            it "should process the list and return as soon as the operation is done" $ do
                channel <- newEmptyMVar
                startProcess $ Process { processType = ProcessMany [theNumber, theAnotherNumber] Completed
                                       , processBroadcast = channelBroadcast channel
                                       , processCancellable = Nothing }
                msg1 <- takeMVar channel
                [Completed 1993, Completed 7] `shouldContain` [msg1]
                msg2 <- takeMVar channel
                [ msg | msg <- [Completed 1993, Completed 7], msg /= msg1] `shouldContain` [msg2]
            describe "Cancelling a process" $ do
                it "should cancel the process given a cancellable function" $ do
                    channel <- newEmptyMVar
                    startProcess $ Process { processType = ProcessMany [theSlowAnswer, theSlowAnswer] Completed
                                           , processBroadcast = channelBroadcast channel
                                           , processCancellable =
                                                Just Cancellable { cancellableMsg = Canceled
                                                                 , cancellableOperation = cancelImmediately }
                                           }
                    msg' <- takeMVar channel
                    msg' `shouldBe` (Canceled)
                    takeMVar channel `shouldThrow` anyException
                it "given two operation one fast and the other slow, only the first is completed" $ do
                    channel <- newEmptyMVar
                    startProcess $ Process { processType = ProcessMany [theAnswer, theSlowAnswer] Completed
                                           , processBroadcast = channelBroadcast channel
                                           , processCancellable =
                                                Just Cancellable { cancellableMsg = Canceled
                                                                 , cancellableOperation = theSlowCancel }
                                           }
                    msg' <- takeMVar channel
                    msg' `shouldBe` (Completed 42)
                    msg'' <- takeMVar channel
                    msg'' `shouldBe` (Canceled)
                    takeMVar channel `shouldThrow` anyException


data Msg
  = Completed Int
  | ProcessedList [Int]
  | Canceled
    deriving (Show, Eq)


shouldBeEqualProcessedList :: [Int] -> Msg -> IO ()
shouldBeEqualProcessedList list msg = do
    case msg of
      ProcessedList values ->
          values `shouldMatchList` list
    return ()


channelBroadcast :: MVar Msg -> Msg -> IO ()
channelBroadcast channel msg = do
    putMVar channel msg
    return ()


theAnswer :: IO Int
theAnswer = do
    return $ 42


theNumber :: IO Int
theNumber = do
    return $ 1993


theSlowAnswer :: IO Int
theSlowAnswer = do
    threadDelay 2000
    return 42


theAnotherNumber :: IO Int
theAnotherNumber = do
    return $ 7


cancelImmediately :: IO Bool
cancelImmediately = do
    return True


neverCancel :: IO Bool
neverCancel = do
    return False


theSlowCancel :: IO Bool
theSlowCancel = do
    threadDelay 1000
    return True
