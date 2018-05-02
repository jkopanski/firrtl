module Parser.Expr where

import Data.Text.Lazy        (Text)
import Data.Void             (Void)
import Parser.Common         (parse, testParser)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Tasty            (TestTree)
import Text.Megaparsec       (ParseError)

import Firrtl.Lo.Syntax.Expr (Expr)
import qualified Firrtl.Lo.Parser
import qualified Firrtl.Lo.Parser.Expr
import qualified Firrtl.Lo.Syntax.Expr as Expr

parseExpr :: Text -> Either (ParseError Char Void) Expr
parseExpr = parse Firrtl.Lo.Parser.Expr.expr

exprParserTests :: TestTree
exprParserTests = testParser "expression parser tests" $ do
  describe "references" $
    it "simple reference" $
      parseExpr "somenode"
      `shouldParse`
      Expr.Ref "somenode"

  describe "literals" $ do
    it "unsigned" $
      parseExpr "UInt(10)"
      `shouldParse`
      Expr.Lit (Expr.UInt 10)

    it "invlid unsigned" $
      parseExpr
      `shouldFailOn`
      "UInt(-2)"

    it "signed pos" $
      parseExpr "SInt(23)"
      `shouldParse`
      Expr.Lit (Expr.SInt 23)

    it "signed neg" $
      parseExpr "SInt(-14)"
      `shouldParse`
      Expr.Lit (Expr.SInt (-14))

    describe "other literals" $ do
      it "unsigned pos hex" $
        parseExpr "UInt(\"hFe\")"
        `shouldParse`
        Expr.Lit (Expr.UInt 254)

      it "unsinged pos oct" $
        parseExpr "UInt(\"o17\")"
        `shouldParse`
        Expr.Lit (Expr.UInt 15)

      it "unsinged pos bin" $
        parseExpr "UInt(\"b1101\")"
        `shouldParse`
        Expr.Lit (Expr.UInt 13)

      it "signed neg hex" $
        parseExpr "SInt(\"-hFe\")"
        `shouldParse`
        Expr.Lit (Expr.SInt (-254))

      it "unsinged neg oct" $
        parseExpr "SInt(\"-o17\")"
        `shouldParse`
        Expr.Lit (Expr.SInt (-15))

      it "unsinged pos bin" $
        parseExpr "SInt(\"-b1101\")"
        `shouldParse`
        Expr.Lit (Expr.SInt (-13))

  describe "conditional & multiplexor" $ do
    it "validif" $
      parseExpr "validif(UInt(1), a)"
      `shouldParse`
      Expr.Valid
        (Expr.Lit (Expr.UInt 1))
        (Expr.Ref "a")

    it "mux" $
      parseExpr "mux(UInt(1), a, b)"
      `shouldParse`
      Expr.Mux
        (Expr.Lit (Expr.UInt 1))
        (Expr.Ref "a")
        (Expr.Ref "b")

  describe "primary operations" $ do
    it "unary" $
      parseExpr "andr(a)"
      `shouldParse`
      Expr.Unary Expr.AndR (Expr.Ref "a")

    it "binary" $
      parseExpr "add(a, b)"
      `shouldParse`
      Expr.Binary Expr.Add (Expr.Ref "a")
                           (Expr.Ref "b")

    it "ternary" $
      parseExpr "bits(a, 1, 3)"
      `shouldParse`
      Expr.Ternary Expr.Bits
        (Expr.Ref "a")
        (Expr.Parameter (Expr.Imm 1))
        (Expr.Parameter (Expr.Imm 3))
