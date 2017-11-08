module ContextSpec where

import Hue.Context
import Test.Hspec

contextSpec :: SpecWith ()
contextSpec =
  describe "Given a context" $
  it "should store a context and return a unique id" $ do
    let contextManager = hueCreateContextManager
    let (contextManager', context1) = hueRegisterContext contextManager "my-context"
    let (_, context2) = hueRegisterContext contextManager' "my-context"
    hueContextId context1 `shouldNotBe` hueContextId context2
