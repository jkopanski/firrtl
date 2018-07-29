module Args where

import Data.Semigroup ((<>))
import Options.Applicative

data Config = Config
  { file :: String }

options :: ParserInfo Config
options = info (config <**> helper)
  ( fullDesc
 <> progDesc "LoFIRRTL interpreter"
 <> header "Interpreter for lowered form of Flexible Intermediate Representation for RTL" )

config :: Parser Config
config = Config <$> input

input :: Parser String
input = strArgument
  ( metavar "FILE"
 <> help "File containing circuit to simulate" )

