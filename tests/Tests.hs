module Main where

import Interpret (interpretTests)
import Parser    (parserTests)
import Test.Tasty

allTests :: TestTree
allTests = testGroup "firrtl tests"
  [ interpretTests
  , parserTests
  ]

main :: IO ()
main = defaultMain allTests
