module ProcessIOSpec where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception.Base
import Hue.Process
import Test.Hspec

processIOSpec :: SpecWith ()
processIOSpec = do
  describe "Given an IO Operation and an update function" $ do
    it "should call the update with the IO result message" $ do
      channel <- newEmptyMVar
      startProcess
        Process
        { processType = ProcessOnly theAnswer Completed
        , processCancellable = Nothing
        }
        (channelBroadcast channel)
      msg' <- takeMVar channel
      msg' `shouldBe` Completed 42
    describe "Cancelling the process" $ do
      it "should cancel the process given an cancel function" $ do
        channel <- newEmptyMVar
        startProcess
          Process
          { processType = ProcessOnly theSlowAnswer Completed
          , processCancellable =
              Just
                Cancellable
                { cancellableMsg = Canceled
                , cancellableOperation = cancelImmediately
                }
          }
          (channelBroadcast channel)
        shouldReceiveJustOne channel Canceled
      it "should execute the process if the cancel function returns false" $ do
        channel <- newEmptyMVar
        startProcess
          Process
          { processType = ProcessOnly theSlowAnswer Completed
          , processCancellable =
              Just
                Cancellable
                {cancellableMsg = Canceled, cancellableOperation = neverCancel}
          }
          (channelBroadcast channel)
        shouldReceiveJustOne channel (Completed 42)
      it "shouldn't send the canceled message if the process already completed" $ do
        channel <- newEmptyMVar
        startProcess
          Process
          { processType = ProcessOnly theAnswer Completed
          , processCancellable =
              Just
                Cancellable
                { cancellableMsg = Canceled
                , cancellableOperation = theSlowCancel
                }
          }
          (channelBroadcast channel)
        shouldReceiveJustOne channel (Completed 42)
  describe "Given a list of IO Operation and an update function" $ do
    describe
      "When I expect the parallel operation to update the full list of result" $ do
      it "should process all the list of IO operation" $ do
        channel <- newEmptyMVar
        startProcess
          Process
          { processType =
              ProcessBulk [theAnswer, theNumber, theAnotherNumber] ProcessedList
          , processCancellable = Nothing
          }
          (channelBroadcast channel)
        msg' <- takeMVar channel
        shouldBeEqualProcessedList [1993, 42, 7] msg'
      it "should do nothing given a empty list" $ do
        channel <- newEmptyMVar
        startProcess
          Process
          { processType = ProcessBulk [] ProcessedList
          , processCancellable = Nothing
          }
          (channelBroadcast channel)
        msg' <- takeMVar channel
        shouldBeEqualProcessedList [] msg'
      describe "Cancelling the process" $ do
        it "should cancel the process given a cancellable function" $ do
          channel <- newEmptyMVar
          startProcess
            Process
            { processType =
                ProcessBulk [theSlowAnswer, theSlowAnswer] ProcessedList
            , processCancellable =
                Just
                  Cancellable
                  { cancellableMsg = Canceled
                  , cancellableOperation = cancelImmediately
                  }
            }
            (channelBroadcast channel)
          shouldReceiveJustOne channel Canceled
        it
          "shouldn't send the canceled message if the process already completed" $ do
          channel <- newEmptyMVar
          startProcess
            Process
            { processType = ProcessBulk [theAnswer, theAnswer] ProcessedList
            , processCancellable =
                Just
                  Cancellable
                  { cancellableMsg = Canceled
                  , cancellableOperation = theSlowCancel
                  }
            }
            (channelBroadcast channel)
          msg' <- takeMVar channel
          shouldBeEqualProcessedList [42, 42] msg'
          takeMVar channel `shouldThrow` anyException
    describe "When I expect the parallel operation to update each operation" $ do
      it "should process the list and return as soon as the operation is done" $ do
        channel <- newEmptyMVar
        startProcess
          Process
          { processType = ProcessMany [theNumber, theAnotherNumber] Completed
          , processCancellable = Nothing
          }
          (channelBroadcast channel)
        msg1 <- takeMVar channel
        [Completed 1993, Completed 7] `shouldContain` [msg1]
        msg2 <- takeMVar channel
        [msg | msg <- [Completed 1993, Completed 7], msg /= msg1] `shouldContain`
          [msg2]
      describe "Cancelling a process" $ do
        it "should cancel the process given a cancellable function" $ do
          channel <- newEmptyMVar
          startProcess
            Process
            { processType = ProcessMany [theSlowAnswer, theSlowAnswer] Completed
            , processCancellable =
                Just
                  Cancellable
                  { cancellableMsg = Canceled
                  , cancellableOperation = cancelImmediately
                  }
            }
            (channelBroadcast channel)
          shouldReceiveJustOne channel Canceled
        it
          "given two operation one fast and the other slow, only the first is completed" $ do
          channel <- newEmptyMVar
          startProcess
            Process
            { processType = ProcessMany [theAnswer, theSlowAnswer] Completed
            , processCancellable =
                Just
                  Cancellable
                  { cancellableMsg = Canceled
                  , cancellableOperation = theSlowCancel
                  }
            }
            (channelBroadcast channel)
          msg' <- takeMVar channel
          msg' `shouldBe` Completed 42
          msg'' <- takeMVar channel
          msg'' `shouldBe` Canceled
          takeMVar channel `shouldThrow` anyException

data Msg
  = Completed Int
  | ProcessedList [Int]
  | Canceled
  deriving (Show, Eq)

shouldReceiveJustOne :: MVar Msg -> Msg -> IO ()
shouldReceiveJustOne channel expectedMsg = do
  msg <- takeMVar channel
  msg `shouldBe` expectedMsg
  takeMVar channel `shouldThrow` anyException
  return ()

shouldBeEqualProcessedList :: [Int] -> Msg -> IO ()
shouldBeEqualProcessedList list msg = do
  case msg of
    ProcessedList values -> values `shouldMatchList` list
  return ()

channelBroadcast :: MVar Msg -> Msg -> IO ()
channelBroadcast channel msg = do
  putMVar channel msg
  return ()

theAnswer :: IO Int
theAnswer = return 42

theNumber :: IO Int
theNumber = return 1993

theSlowAnswer :: IO Int
theSlowAnswer = do
  threadDelay 2000
  return 42

theAnotherNumber :: IO Int
theAnotherNumber = return 7

cancelImmediately :: IO Bool
cancelImmediately = return True

neverCancel :: IO Bool
neverCancel = return False

theSlowCancel :: IO Bool
theSlowCancel = do
  threadDelay 1000
  return True
