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
      parseExpr "UInt<4>(10)"
      `shouldParse`
      Expr.Lit (Expr.UInt (Just 4) 10)

    it "invlid unsigned" $
      parseExpr
      `shouldFailOn`
      "UInt<3>(-2)"

    it "signed pos" $
      parseExpr "SInt<5>(23)"
      `shouldParse`
      Expr.Lit (Expr.SInt (Just 5) 23)

    it "signed neg" $
      parseExpr "SInt(-14)"
      `shouldParse`
      Expr.Lit (Expr.SInt Nothing (-14))

    describe "other literals" $ do
      it "unsigned pos hex" $
        parseExpr "UInt(\"hFe\")"
        `shouldParse`
        Expr.Lit (Expr.UInt Nothing 254)

      it "unsinged pos oct" $
        parseExpr "UInt<16>(\"o17\")"
        `shouldParse`
        Expr.Lit (Expr.UInt (Just 16) 15)

      it "unsinged pos bin" $
        parseExpr "UInt(\"b1101\")"
        `shouldParse`
        Expr.Lit (Expr.UInt Nothing 13)

      it "signed neg hex" $
        parseExpr "SInt<9>(\"-hFe\")"
        `shouldParse`
        Expr.Lit (Expr.SInt (Just 9) (-254))

      it "unsinged neg oct" $
        parseExpr "SInt<17>(\"-o17\")"
        `shouldParse`
        Expr.Lit (Expr.SInt (Just 17) (-15))

      it "unsinged pos bin" $
        parseExpr "SInt(\"-b1101\")"
        `shouldParse`
        Expr.Lit (Expr.SInt Nothing (-13))

  describe "conditional & multiplexor" $ do
    it "validif" $
      parseExpr "validif(UInt(1), a)"
      `shouldParse`
      Expr.Valid
        (Expr.Lit (Expr.UInt Nothing 1))
        (Expr.Ref "a")

    it "mux" $
      parseExpr "mux(UInt(1), a, b)"
      `shouldParse`
      Expr.Mux
        (Expr.Lit (Expr.UInt Nothing 1))
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
