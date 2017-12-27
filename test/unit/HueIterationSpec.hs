module HueIterationSpec where

import Hue.Context
import Hue.Iteration
import Test.Hspec


data Msg = MyCoolMsg | ChangedMsg Int
  deriving (Show, Eq)


data State =
  State { anyNumber :: Int
        , anyString :: String }
  deriving (Show, Eq)


updater :: HueIterationUpdater Msg String State
updater context state msg =
  return state


hueIterationSpec :: SpecWith ()
hueIterationSpec =
  describe "Given a context and a state message" $ do
    let givenContext = HueContext { hueContextId = 1 }
    let givenState = State { anyNumber = 0, anyString = "Any String"}
    let givenMsg = MyCoolMsg

    let performUpdate = huePerformIteration givenContext givenState givenMsg

    let ioOperation1 = do
          return (42, "The answer")
    let ioOperation2 = do
          return "I don't know the answer"

    describe "Changing state" $ do
      describe "When I perform a change state operation" $ do
        let iterationResult = performUpdate $ \currrentContext currentState currentMsg -> do
              return currentState { anyString = "here" }
        it "should return a new Monad with the new state" $ do
          hueIterationStateResult iterationResult `shouldBe` State 0 "here"
      describe "when I perform the change state action many times" $ do
        let iterationResult = performUpdate $ \currentContext currentState currentMsg -> do
              currentState <- return $ currentState { anyNumber = 42 }
              return currentState { anyString = "here" }
        it "should return the last state" $ do
          hueIterationStateResult iterationResult `shouldBe` State 42 "here"
    describe "Performing an IO iteration" $ do
      describe "When I perform an IO operation" $ do
        let ioOperation = do
              return (42, "The answer")
        let iterationResult = performUpdate $ \currentContext currentState currentMsg -> do
              huePerformTask ioOperation $ \currentState (number, text) -> do
                return $ State number text
              return $ currentState
        it "Should register the given task" $ do
          let task = head $ hueIterationTasksResult iterationResult
          ioIterationResult <- task givenState
          hueIterationStateResult (hueIterationToResult ioIterationResult) `shouldBe` State 42 "The answer"
      describe "Given many IO operations" $ do
        let iterationResult = performUpdate $ \currentContext currentState currentMsg -> do
              huePerformTask ioOperation1 $ \currentState (number, text) -> do
                return $ State number text
              return "any thing that between the tasks"
              huePerformTask ioOperation2 $ \currentState text -> do
                return $ State (-1) text
              return $ currentState
        it "Should register the given tasks" $ do
          let task1 = (hueIterationTasksResult iterationResult) !! 0
          let task2 = (hueIterationTasksResult iterationResult) !! 1
          ioIterationResult1 <- task1 givenState
          hueIterationStateResult (hueIterationToResult ioIterationResult1) `shouldBe` State 42 "The answer"
          ioIterationResult2 <- task2 givenState
          hueIterationStateResult (hueIterationToResult ioIterationResult2) `shouldBe` State (-1) "I don't know the answer"
    describe "Responding to the context" $ do
      describe "When I perform the respond operation to the given context" $ do
        let iterationResult = performUpdate $ \currentContext currentState currentMsg -> do
              hueRespond currentContext "Pong"
              return $ State 42 "OK"
        it "should register the context response" $ do
          let response = head (hueIterationResponsesResult iterationResult)
          fst response `shouldBe` givenContext
          snd response `shouldBe` "Pong"
    describe "Performing many operations" $ do
      describe "When I perform many and different operations" $ do
        let iterationResult = performUpdate $ \currentContext currentState currentMsg -> do
              huePerformTask ioOperation1 $ \currentState (number, text) -> do
                return $ State number text
              return "any thing that between the tasks"
              hueRespond currentContext "OK"
              huePerformTask ioOperation2 $ \currentState text -> do
                return $ State (-1) text
              return $ State 31 "All done"
        it "Should register all tasks and responses correctly" $ do
          length (hueIterationTasksResult iterationResult) `shouldBe` 2
          length (hueIterationResponsesResult iterationResult) `shouldBe` 1
          hueIterationStateResult iterationResult `shouldBe` State 31 "All done"
