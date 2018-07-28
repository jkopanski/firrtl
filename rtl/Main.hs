module Main where

import Prelude hiding (readFile)
import           Data.Text.Lazy.IO   (readFile)
import           Options.Applicative (execParser)
import qualified Firrtl.Lo.Parser    as Parser
import qualified Firrtl.Lo.Syntax    as Syntax
import qualified Firrtl.Lo.TypeCheck as Check
import qualified Text.Megaparsec

import Args (file, options)

main :: IO ()
main = do
  opts <- execParser options
  let fname = file opts
  text <- readFile fname
  ast <- case Parser.parse fname text of
    Left e -> fail $ Text.Megaparsec.parseErrorPretty' text e
    Right (Syntax.Top ast) -> pure ast

  case Check.check ast of
    Right _  -> pure ()
    Left err -> fail $ show err

  putStrLn "Ok!"
