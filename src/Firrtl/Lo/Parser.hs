module Firrtl.Lo.Parser (parse) where

import qualified Text.Megaparsec
import Data.Text.Lazy           (Text)
import Data.Void                (Void)
import Firrtl.Lo.Parser.Circuit (circuit)
import Firrtl.Lo.Parser.Monad   as P

import Firrtl.Lo.Syntax as S

parse
  :: String
  -> Text
  -> Either (Text.Megaparsec.ParseError Char Void) S.Syntax
parse name text = case result of
  Left l -> Left l
  Right ast -> Right $ S.Top ast

  where result = Text.Megaparsec.parse (P.unParser parser) name text
        parser = circuit <* Text.Megaparsec.eof
