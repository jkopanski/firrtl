module Parser.Stmt where

import Data.Text.Lazy        (Text)
import Data.Void             (Void)
import Parser.Common         (parse, testParser)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Tasty            (TestTree)
import Text.Megaparsec       (ParseError)

import Firrtl.Lo.Syntax.Stmt (Stmt)
import qualified Firrtl.Lo.Syntax as Syntax
import qualified Firrtl.Lo.Parser
import qualified Firrtl.Lo.Parser.Stmt
import qualified Firrtl.Lo.TypeCheck.Ty as Types

parseStmt :: Text -> Either (ParseError Char Void) Stmt
parseStmt = parse Firrtl.Lo.Parser.Stmt.stmt

stmtParserTests :: TestTree
stmtParserTests = testParser "statemet parser tests" $ do
  describe "connect" $ do
    it "standard" $
      parseStmt "somenode <= UInt<4>(15)"
      `shouldParse`
      Syntax.Connect
        (Syntax.Ref "somenode")
        (Syntax.Lit (Syntax.UInt (Just 4) 15))

    it "partial" pending
      -- parseStmt "somenode <- SInt(-2)"
      -- `shouldParse`
      -- Syntax.Partial
      --   (Syntax.Ref "somenode")
      --   (Syntax.Lit (Syntax.SInt -2))

  it "empty" $
    parseStmt "skip"
    `shouldParse`
    Syntax.Empty

  it "invalid" $
    parseStmt "somenode is invalid"
    `shouldParse`
    Syntax.Invalid (Syntax.Ref "somenode")

  it "node" $
    parseStmt "node a = UInt<4>(15)"
    `shouldParse`
    Syntax.Node "a" (Syntax.Lit (Syntax.UInt (Just 4) 15))

  describe "printfs" $ do
    it "without optional args" $
      parseStmt "printf(UInt(1), UInt(1), \"Test\")"
      `shouldParse`
      Syntax.Print (Syntax.Lit (Syntax.UInt Nothing 1))
                   (Syntax.Lit (Syntax.UInt Nothing 1))
                   "Test"
                   []

    it "with 1 optional arg" $
      parseStmt "printf(UInt(1), UInt<1>(1), \"Test %d\", UInt<2>(2))"
      `shouldParse`
      Syntax.Print (Syntax.Lit (Syntax.UInt Nothing 1))
                   (Syntax.Lit (Syntax.UInt (Just 1) 1))
                   "Test %d"
                   [Syntax.Lit (Syntax.UInt (Just 2) 2)]

    it "with multiple optional arg" $
      parseStmt "printf(UInt(1), UInt(1), \"Test %d %d %d\", UInt<2>(2), UInt<4>(15), SInt<4>(-3))"
      `shouldParse`
      Syntax.Print (Syntax.Lit (Syntax.UInt Nothing 1))
                   (Syntax.Lit (Syntax.UInt Nothing 1))
                   "Test %d %d %d"
                   [ Syntax.Lit (Syntax.UInt (Just 2) 2)
                   , Syntax.Lit (Syntax.UInt (Just 4) 15)
                   , Syntax.Lit (Syntax.SInt (Just 4) (-3))
                   ]

    it "stop" $
      parseStmt "stop(UInt<2>(2), UInt<4>(13), -4)"
      `shouldParse`
      Syntax.Stop (Syntax.Lit (Syntax.UInt (Just 2) 2))
                  (Syntax.Lit (Syntax.UInt (Just 4) 13))
                  (-4)

    describe "wires" $ do
      it "unsigned" $
        parseStmt "wire drut: UInt<14>"
        `shouldParse`
        Syntax.Wire "drut" (Types.Unsigned, 14, Types.Bi)

      it "signed" $
        parseStmt "wire druciak : SInt<2>"
        `shouldParse`
        Syntax.Wire "druciak" (Types.Signed, 2, Types.Bi)

      it "clock" $
        parseStmt "wire clk: Clock"
        `shouldParse`
        Syntax.Wire "clk" (Types.Clock, 1, Types.Bi)
