module Parser where

import Test.Tasty (TestTree)
import qualified Test.Tasty
import Parser.Circuit
import Parser.Expr
import Parser.Stmt

parserTests :: TestTree
parserTests = Test.Tasty.testGroup "parser tests"
  [ circParserTests
  , exprParserTests
  , stmtParserTests
  ]
