module Interpret where

import Test.Tasty (TestTree)
import qualified Test.Tasty
import Interpret.Eval

interpretTests :: TestTree
interpretTests = Test.Tasty.testGroup "interpreter tests"
  [ evalExprTests
  ]
