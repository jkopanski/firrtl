module Firrtl.Lo.Pretty.Common where

import           Data.Text.Prettyprint.Doc                 (Doc)
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal

data Ann
  = Ground   -- ^ Ground types
  | Keyword
  | Literal
  | Reference
  | Syntax

annToAnsiStyle :: Ann -> Terminal.AnsiStyle
annToAnsiStyle Ground    = Terminal.bold <> Terminal.colorDull Terminal.Green
annToAnsiStyle Keyword   = Terminal.bold <> Terminal.colorDull Terminal.Green
annToAnsiStyle Literal   = Terminal.colorDull Terminal.Magenta
annToAnsiStyle Reference = Terminal.colorDull Terminal.Blue
annToAnsiStyle Syntax    = Terminal.bold <> Terminal.colorDull Terminal.Cyan

ground, keyword, literal, reference, syntax :: Doc Ann -> Doc Ann
ground    = Pretty.annotate Ground
keyword   = Pretty.annotate Keyword
literal   = Pretty.annotate Literal
reference = Pretty.annotate Reference
syntax    = Pretty.annotate Syntax

comma, langle, rangle, lbrace, rbrace, lbraket, rbraket, lparen, rparen, dot :: Doc Ann
comma   = syntax Pretty.comma
langle  = syntax Pretty.langle
rangle  = syntax Pretty.rangle
lbrace  = syntax Pretty.lbrace
rbrace  = syntax Pretty.rbrace
lbraket = syntax Pretty.lbraket
rbraket = syntax Pretty.rbraket
lparen  = syntax Pretty.lparen
rparen  = syntax Pretty.rparen
dot     = syntax "."
  
