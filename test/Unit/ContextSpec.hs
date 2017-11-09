module ContextSpec where

import Hue.Context
import Test.Hspec


contextSpec :: SpecWith ()
contextSpec =
  let contextManager = hueCreateContextManager
      (contextManager', context) =
        hueRegisterContext contextManager "my-context"
      fakeId = 123
  in describe "Given a context" $ do
       describe "Creating a context" $
         it "should store a context and return a unique id" $ do
           let (_, context2) = hueRegisterContext contextManager' "my-context"
           hueContextId context `shouldNotBe` hueContextId context2
       describe "Getting a contex" $ do
         it "should get a context by id" $ do
           let expectedContext =
                 hueGetContextById contextManager' $ hueContextId context
           expectedContext `shouldBe` Just context
         it "return nothing for a non existent id" $ do
           let expectedContext = hueGetContextById contextManager' fakeId
           expectedContext `shouldBe` Nothing
       describe "taking a context" $ do
         let (contextManager'', expectedContext) =
               hueTakeContextById contextManager' (hueContextId context)
         it "should get context from the context manager" $
           expectedContext `shouldBe` Just context
         it "should remove the context from the context manager" $ do
           let expectedContextPickedAgain = hueGetContextById contextManager'' $ hueContextId context
           expectedContextPickedAgain `shouldBe` Nothing
         it "return nothing for a non existent id" $ do
           let (contextManager'', expectedContext) = hueTakeContextById contextManager' fakeId
           expectedContext `shouldBe` Nothing
       describe "check for context changes" $ do
         it "should return false given that the context was not changed" $
           hueHasContextChange contextManager' (hueContextId context) context `shouldBe`
           Right False
         it "should return true when the context is changed" $ do
           let changedContext = context {hueContext = "my-context-changed"}
           hueHasContextChange
             contextManager'
             (hueContextId changedContext)
             changedContext `shouldBe`
             Right True
