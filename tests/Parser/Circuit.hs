module Parser.Circuit where

import Data.List.NonEmpty    (NonEmpty((:|)))
import Data.Text.Lazy        (Text)
import Data.Void             (Void)
import Parser.Common         (parse, testParser)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Tasty            (TestTree)
import Text.Megaparsec       (ParseError)

import Firrtl.Lo.Syntax (Circuit, Module)
import qualified Firrtl.Lo.Syntax as Syntax
import qualified Firrtl.Lo.Parser
import qualified Firrtl.Lo.Parser.Circuit
import Firrtl.Lo.TypeCheck.Ty -- pes as Types

parseCirc :: Text -> Either (ParseError Char Void) Circuit
parseCirc = parse Firrtl.Lo.Parser.Circuit.circuit

ex00 :: Text -> Module
ex00 name = Syntax.ExtModule name
  [ Syntax.Port "a"   (Unsigned, 5, Male)
  , Syntax.Port "b"   (Signed,   1, Female)
  , Syntax.Port "clk" (Clock,    1, Male)
  ]

ex01 :: Text -> Module
ex01 name = Syntax.Module name
  [ Syntax.Port "a"   (Unsigned, 5, Male)
  , Syntax.Port "b"   (Signed,   1, Female)
  , Syntax.Port "clk" (Clock,    1, Male)
  ]
  (Syntax.Block [Syntax.Empty])

circParserTests :: TestTree
circParserTests = testParser "circuit parser tests" $ do
  describe "circuits" $ do
    it "single extmodule" $
      parseCirc "circuit Top  :       \n\
                \  extmodule Top :    \n\
                \    input a: UInt<5> \n\
                \    output b: SInt<1>\n\
                \    input clk: Clock \n"
      `shouldParse`
       Syntax.Circuit "Top" (ex00 "Top" :| [])

    it "2 extmodules" $
      parseCirc "circuit Top  :       \n\
                \  extmodule Top :    \n\
                \    input a: UInt<5> \n\
                \    output b: SInt<1>\n\
                \    input clk: Clock \n\
                \  extmodule Demo :   \n\
                \    input a: UInt<5> \n\
                \    output b: SInt<1>\n\
                \    input clk: Clock \n"
      `shouldParse`
      Syntax.Circuit "Top" (ex00 "Top" :| [ex00 "Demo"])

    it "single module" $
      parseCirc "circuit Top :        \n\
                \  module Top :       \n\
                \    input a: UInt<5> \n\
                \    output b: SInt<1>\n\
                \    input clk: Clock \n\
                \    skip             \n"
      `shouldParse`
      Syntax.Circuit "Top" (ex01 "Top" :| [])

    it "2 modules" $
      parseCirc "circuit Top :        \n\
                \  module Top :       \n\
                \    input a: UInt<5> \n\
                \    output b: SInt<1>\n\
                \    input clk: Clock \n\
                \    skip             \n\
                \                     \n\
                \  module Demo :      \n\
                \    input a: UInt<5> \n\
                \    output b: SInt<1>\n\
                \    input clk: Clock \n\
                \    skip             \n"
      `shouldParse`
      Syntax.Circuit "Top" (ex01 "Top" :| [ex01 "Demo"])

    it "no ports" $
      parseCirc "circuit Top :        \n\
                \  module Top :       \n\
                \    skip             \n"
      `shouldParse`
      Syntax.Circuit "Top"
        (Syntax.Module "Top" [] (Syntax.Block [Syntax.Empty]) :| [])

  describe "invalid circuits" $ do
    it "no module indentation" $
      parseCirc
      `shouldFailOn`
      "circuit Top :        \n\
      \module Top :         \n\
      \    input a: UInt<5> \n\
      \    output b: SInt<1>\n\
      \    input clk: Clock \n\
      \    skip             \n"

    it "no body indentation" $
      parseCirc
      `shouldFailOn`
      "circuit Top :        \n\
      \  module Top :       \n\
      \  input a: UInt<5>   \n\
      \  output b: SInt<1>  \n\
      \  input clk: Clock   \n\
      \  skip               \n"

  describe "formatting" $ do
    it "no newline at the end" $
      parseCirc "circuit Top :        \n\
                \  module Top :       \n\
                \    input a: UInt<5> \n\
                \    output b: SInt<1>\n\
                \    input clk: Clock \n\
                \    skip             "
      `shouldParse`
      Syntax.Circuit "Top" (ex01 "Top" :| [])

    it "multiple newlines at the end" $
      parseCirc "circuit Top :        \n\
                \  module Top :       \n\
                \    input a: UInt<5> \n\
                \    output b: SInt<1>\n\
                \    input clk: Clock \n\
                \                     \n\
                \    skip             \n\
                \                     \n\
                \                     \n"
      `shouldParse`
      Syntax.Circuit "Top" (ex01 "Top" :| [])

    it "newlines at the beginning" $
      parseCirc "                     \n\
                \                     \n\
                \                     \n\
                \circuit Top :        \n\
                \  module Top :       \n\
                \    input a: UInt<5> \n\
                \    output b: SInt<1>\n\
                \    input clk: Clock \n\
                \    skip             \n"
      `shouldParse`
      Syntax.Circuit "Top" (ex01 "Top" :| [])

    it "body at different indent than ports" $
      parseCirc
      `shouldFailOn`
      "circuit Top :          \n\
      \  module Top :         \n\
      \    input a: UInt<5>   \n\
      \    output b: SInt<1>  \n\
      \    input clk: Clock   \n\
      \      skip             \n"
