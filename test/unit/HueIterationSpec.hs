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

hueIterationSpec :: SpecWith ()
hueIterationSpec =
  describe "Given a context and a state message" $ do
    let givenContext = HueContext { hueContextId = 1 }
    let givenState = State { anyNumber = 0, anyString = "Any String"}
    let givenMsg = MyCoolMsg

    let ioOperation1 = do
          return (42, "The answer")
    let ioOperation2 = do
          return "Tell me why I don't like Mondays."

    let performUpdate = huePerformIteration givenContext givenState givenMsg

    describe "Checking the given update arguments" $
      it "should receive the given state and message" $ do
        let iterationResult = performUpdate $ \_ currentState currentMsg ->
              if currentState == givenState && currentMsg == givenMsg
                 then return $ State 1 "ok"
              else return $ State 0 "The state or msg is not the given"
        hueIterationStateResult iterationResult `shouldBe` State 1 "ok"
    describe "Changing state" $ do
      describe "When I perform a change state operation" $ do
        let iterationResult = performUpdate $ \_ currentState _ -> do
              return currentState { anyString = "here" }
        it "should return a new Monad with the new state" $ do
          hueIterationStateResult iterationResult `shouldBe` State 0 "here"
          length (hueIterationTasksResult iterationResult) `shouldBe` 0
          length (hueIterationResponsesResult iterationResult) `shouldBe` 0
      describe "when I perform the change state action many times" $ do
        let iterationResult = performUpdate $ \_ currentState _ -> do
              newCurrentState <- return $ currentState { anyNumber = 42 }
              return newCurrentState { anyString = "here" }
        it "should return the last state" $ do
          hueIterationStateResult iterationResult `shouldBe` State 42 "here"
    describe "Performing an IO iteration" $ do
      describe "When I perform an IO operation" $ do
        let ioOperation = do
              return (42, "The answer")
        let iterationResult = performUpdate $ \_ currentState _ -> do
              huePerformTask ioOperation $ \_ (number, text) -> do
                return $ State number text
              return $ currentState
        it "Should register the given task" $ do
          let task = head $ hueIterationTasksResult iterationResult
          ioIterationTask <- task
          let ioIterationResult = ioIterationTask givenState
          hueIterationStateResult (hueIterationToResult ioIterationResult) `shouldBe` State 42 "The answer"
      describe "Given many IO operations" $ do
        let iterationResult = performUpdate $ \_ currentState _ -> do
              huePerformTask ioOperation1 $ \_ (number, text) -> do
                return $ State number text
              _ <- return "any thing that between the tasks"
              huePerformTask ioOperation2 $ \_ text -> do
                return $ State (-1) text
              return $ currentState
        it "Should register the given tasks" $ do
          task1 <- (hueIterationTasksResult iterationResult) !! 0
          task2 <- (hueIterationTasksResult iterationResult) !! 1
          let ioIterationResult1 = task1 givenState
          hueIterationStateResult (hueIterationToResult ioIterationResult1) `shouldBe` State 42 "The answer"
          let ioIterationResult2 = task2 givenState
          hueIterationStateResult (hueIterationToResult ioIterationResult2) `shouldBe` State (-1) "Tell me why I don't like Mondays."
      describe "When I call a task providing an state" $ do
        let ioOperation = return "The answer"
        let iterationResult = performUpdate $ \_ currentState _ -> do
              huePerformTask ioOperation $ \newCurrentState _ -> do
                return newCurrentState
              return $ currentState
        it "should receive the given state" $ do
          let task = head $ hueIterationTasksResult iterationResult
          ioIterationTask <- task
          let ioIterationResult = ioIterationTask $ State 1 "My given state"
          hueIterationStateResult (hueIterationToResult ioIterationResult) `shouldBe` State 1 "My given state"
    describe "Responding to the context" $ do
      describe "When I perform the respond operation to the given context" $ do
        let iterationResult = performUpdate $ \resolver _ _ -> do
              resolver "Pong"
              return $ State 42 "OK"
        it "should register the context response" $ do
          let response = head (hueIterationResponsesResult iterationResult)
          fst response `shouldBe` givenContext
          snd response `shouldBe` "Pong"
    describe "Performing many operations" $ do
      describe "When I perform many and different operations" $ do
        let iterationResult = performUpdate $ \resolver _ _ -> do
              huePerformTask ioOperation1 $ \_ (number, text) -> do
                return $ State number text
              _ <- return "any thing that between the tasks"
              resolver "OK"
              huePerformTask ioOperation2 $ \_ text -> do
                return $ State (-1) text
              return $ State 31 "All done"
        it "Should register all tasks and responses correctly" $ do
          length (hueIterationTasksResult iterationResult) `shouldBe` 2
          length (hueIterationResponsesResult iterationResult) `shouldBe` 1
          hueIterationStateResult iterationResult `shouldBe` State 31 "All done"
    describe "Using the HueIteration as a Applicative" $ do
      describe "When I do a sequence application" $ do
        describe "And it's a identity application" $ do
          let (HueIteration iterationData result) =
                pure id <*> HueIteration (HueIterationData [] []) "Hi"

          it "should return the same result and the data constructor should be HueIteration" $ do
            result `shouldBe` "Hi"
            length (hueIterationTasks iterationData) `shouldBe` 0
        describe "And both has iteration tasks" $ do
          let functionIteration = do
                huePerformTask ioOperation1 $ \state _ -> return state
                huePerformTask ioOperation1 $ \state _ -> return state
                return id
          let anotherIteration = do
                huePerformTask ioOperation1 $ \state _ -> return state
                huePerformTask ioOperation1 $ \state _ -> return state
                return $ State 42 "All fine"
          let (HueIteration iterationData result) = functionIteration <*> anotherIteration

          it "should return the same result with both iteration data" $ do
            result `shouldBe` (State 42 "All fine")
            length (hueIterationTasks iterationData) `shouldBe` 4
