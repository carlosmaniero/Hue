module ProcessIOSpec where

import Helm.Process
import Test.Hspec
import Control.Concurrent
import Control.Concurrent.MVar


processIOSpec :: SpecWith ()
processIOSpec = do
    describe "Given an IO Operation and an update function" $ do
        it "should call the update with the IO result message" $ do
            channel <- newEmptyMVar
            forkIO $ processIO (channelBroadcast channel) theAnswer Completed
            msg' <- takeMVar channel
            msg' `shouldBe` (Completed 42)

    describe "Given a list of IO Operation and an update function" $ do
        describe "When I expect the parallel operation to update the full list of result" $ do
            it "should process all the list of IO operation" $ do
                channel <- newEmptyMVar
                forkIO $ processIOBulkParallel (channelBroadcast channel) [theNumber, theAnotherNumber] ProcessedList
                msg' <- takeMVar channel
                shouldBeEqualProcessedList [1993, 7] msg'
            it "should do nothing given a empty list" $ do
                channel <- newEmptyMVar
                forkIO $ processIOBulkParallel (channelBroadcast channel) [] ProcessedList
                msg' <- takeMVar channel
                shouldBeEqualProcessedList [] msg'
        describe "When I expect the parallel operation to update each operation" $ do
            it "should process the list and return as soon as the operation is done" $ do
                channel <- newEmptyMVar
                forkIO $ processIOParallel (channelBroadcast channel) [theNumber, theAnotherNumber] Completed
                msg1 <- takeMVar channel
                [Completed 1993, Completed 7] `shouldContain` [msg1]
                msg2 <- takeMVar channel
                [ msg | msg <- [Completed 1993, Completed 7], msg /= msg1] `shouldContain` [msg2]


data Msg
  = Completed Int
  | ProcessedList [Int]
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


theAnotherNumber :: IO Int
theAnotherNumber = do
    return $ 7
