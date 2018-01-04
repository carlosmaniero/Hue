module HueRunnerSpec where

import Hue.Iteration
import Hue.Runner
import Control.Exception
import Test.Hspec


expect42 :: Runner Int -> IO ()
expect42 runner = do
  (Finished result) <- wait runner
  result `shouldBe` 42


sumStateIteration :: Int -> Int -> StateIteration Int Int
sumStateIteration state result = return (state + result)


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
        expect42 runner
    describe "When nested task are performed" $
      it "should complete all nested tasks before return" $ do
        let task state = do
              process (return 1) sumStateIteration
              process (return 2) $ \stateNew response -> do
                process (return 3) sumStateIteration
                return (stateNew + response)
              return state
        runner <- startIteration task (36 :: Int)
        expect42 runner
    describe "When a task fail" $
      it "should be completed with the status no more tasks and the last updated result" $ do
        let task state = do
              process (return 1) sumStateIteration
              process (throwIO (TypeError "Keep out")) $ \_ response -> return response
              process (return 2) sumStateIteration
              return state
        runner <- startIteration task (39 :: Int)
        expect42 runner

    describe "When I stop the runner" $
      it "should stop the running and return the last updated status" $ do
        let task state = do
              process (return 1) $ \stateNew response -> return (stateNew + response)
              return state
        runner <- startIteration task (42 :: Int)
        stop runner
        (Stopped result) <- wait runner
        result `shouldBe` 42
