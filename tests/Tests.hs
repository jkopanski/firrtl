module Main where

import Parser (parserTests)
import Test.Tasty

allTests :: TestTree
allTests = testGroup "firrtl tests" [ parserTests ]

main :: IO ()
main = defaultMain allTests
