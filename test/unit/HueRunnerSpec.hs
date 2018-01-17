module HueRunnerSpec where

import Hue.Iteration
import Hue.Runner
import Control.Exception
import Control.Concurrent
import Test.Hspec


expect42 :: Runner  Int -> RunnerFinishedStatus -> IO ()
expect42 runner status = do
  (result, resultStatus) <- wait runner
  result `shouldBe` 42
  resultStatus `shouldBe` status


sumStateIteration :: Int -> Int -> StateIteration Int
sumStateIteration state result = return (state + result)

startIteration :: TaskIteration state -> state -> IO (Runner state)
startIteration task state = do
  runner <- startRunner state
  schedule runner task
  return runner

hueRunnerSpec :: SpecWith ()
hueRunnerSpec =
  describe "Performing many IO operations" $ do
    describe "When I perform two async tasks and both are processed successfully" $
      it "should be completed with the status finished and the correct result" $ do
        let task state = do
              process (return 1) sumStateIteration
              process (return 2) sumStateIteration
              return state
        runner <- startIteration task (39 :: Int)
        expect42 runner Completed
    describe "When nested task are performed" $
      it "should complete all nested tasks before return" $ do
        let task state = do
              process (return 1) sumStateIteration
              process (return 2) $ \stateNew response -> do
                process (return 3) sumStateIteration
                return (stateNew + response)
              return state
        runner <- startIteration task (36 :: Int)
        expect42 runner Completed
    describe "When a task fail" $
      it "should be completed with the status no more tasks and the last updated result" $ do
        let task state = do
              process (return 1) sumStateIteration
              process (throwIO (TypeError "Keep out")) $ \_ response -> return response
              process (return 2) sumStateIteration
              return state
        runner <- startIteration task (39 :: Int)
        expect42 runner Completed

    describe "When I stop the runner" $
      it "should stop the running and return the last updated status" $ do
        let task state = do
              process (return 1) $ \stateNew response -> return (stateNew + response)
              return state
        runner <- startIteration task (42 :: Int)
        stop runner
        expect42 runner Stopped

    describe "When a task is finished by the finished status" $
      it "should ignore the execution of tasks and return the finish state" $ do
        let task state = do
              process (return 1) sumStateIteration
              _ <- finish 42
              return state
        runner <- startIteration task (1 :: Int)
        expect42 runner Finished
    describe "When I perform the finish in a subprocess it should return the result of the finish" $
      it "should return the result of the finish" $ do
        let task state = do
              process (return 1) $ \newState response -> finish (newState + response)
              process (threadDelay 10000 >>= \_ -> return 2) $ \newState response -> finish (newState + response)
              return state
        runner <- startIteration task (41 :: Int)
        expect42 runner Finished
    describe "When I perform an infinite runner" $
      it "should raise an exception if it has nobody to send new messages" $ do
        runner <- startForeverRunner (42 :: Int)
        schedule runner return
        wait runner `shouldThrow` anyException
