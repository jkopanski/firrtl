module Main where

import Prelude hiding (readFile)
import qualified Data.Text.Lazy.IO   as T
import           Options.Applicative (execParser)
import qualified Firrtl.Lo.Parser    as Parser
import qualified Firrtl.Lo.Pretty    as Pretty
import qualified Firrtl.Lo.Syntax    as Syntax
import qualified Firrtl.Lo.TypeCheck as Check
import qualified Text.Megaparsec

import Args (file, options)

main :: IO ()
main = do
  opts <- execParser options
  let fname = file opts
  text <- T.readFile fname
  ast <- case Parser.parse fname text of
    Left e -> fail $ Text.Megaparsec.parseErrorPretty' text e
    Right (Syntax.Top ast) -> pure ast

  case Check.check ast of
    Right typed -> T.putStrLn $ Pretty.terminal typed
    Left err    -> fail $ show err

  putStrLn "Ok!"
