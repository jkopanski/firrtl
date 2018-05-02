module Args where

import Data.Semigroup ((<>))
import Options.Applicative

data Config = Config
  { file :: String }

options :: ParserInfo Config
options = info (config <**> helper)
  ( fullDesc
 <> progDesc "IOHK test task"
 <> header "node - send and receive messages" )

config :: Parser Config
config = Config <$> input

input :: Parser String
input = strArgument
  ( metavar "FILE"
 <> help "File containing circuit to simulate" )

