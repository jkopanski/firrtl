module Parser.Common where

import           Data.Text.Lazy        (Text)
import           Data.Void             (Void)
import           Test.Hspec       (Spec)
import           Test.Tasty       (TestTree)
import           Text.Megaparsec  (ParseError)
import           System.IO.Unsafe (unsafePerformIO)

import qualified Test.Tasty
import qualified Test.Tasty.Hspec
import qualified Text.Megaparsec
import qualified Firrtl.Lo.Parser
import qualified Firrtl.Lo.Parser.Monad

parse
  :: Firrtl.Lo.Parser.Monad.Parser a
  -> Text
  -> Either (ParseError Char Void) a
parse p = Text.Megaparsec.parse (Firrtl.Lo.Parser.Monad.unParser p) ""

testParser :: String -> Spec -> TestTree
testParser name assert =
  Test.Tasty.testGroup name
  $ unsafePerformIO
  $ Test.Tasty.Hspec.testSpecs assert
