module ContextSpec where

import Hue.Context
import Test.Hspec

contextSpec :: SpecWith ()
contextSpec =
  describe "Given a context" $ do
    it "should store a context and return a unique id" $ do
      let contextManager = hueCreateContextManager
      let (contextManager', context1) =
            hueRegisterContext contextManager "my-context"
      let (_, context2) = hueRegisterContext contextManager' "my-context"
      hueContextId context1 `shouldNotBe` hueContextId context2
    it "should get a context by id" $ do
      let contextManager = hueCreateContextManager
      let (contextManager', context) =
            hueRegisterContext contextManager "my-context"
      let expectedContext =
            hueGetContextById contextManager' $ hueContextId context
      expectedContext `shouldBe` Just context
    describe "check for context changes" $ do
      it "should return false given that the context was not changed" $ do
        let contextManager = hueCreateContextManager
        let (contextManager', context) =
              hueRegisterContext contextManager "my-context"
        hueHasContextChange contextManager' (hueContextId context) context `shouldBe`
          Right False
      it "should return true when the context is changed" $ do
        let contextManager = hueCreateContextManager
        let (contextManager', context) =
              hueRegisterContext contextManager "my-context"
        let changedContext = context {hueContext = "my-context-changed"}
        hueHasContextChange
          contextManager'
          (hueContextId changedContext)
          changedContext `shouldBe`
          Right True
