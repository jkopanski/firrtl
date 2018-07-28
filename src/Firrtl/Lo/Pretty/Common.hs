{-# language
        DataKinds
      , TypeInType #-}
module Firrtl.Lo.Pretty.Common where

import           Data.Nat
import           Data.Singletons.Prelude

import           Data.Text.Prettyprint.Doc                 (Doc)
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal

import Firrtl.Lo.TypeCheck.Ty

data Ann
  = Ground   -- ^ Ground types
  | Keyword
  | Literal
  | Reference
  | Syntax

annToAnsiStyle :: Ann -> Terminal.AnsiStyle
annToAnsiStyle Ground    = Terminal.bold <> Terminal.colorDull Terminal.Green
annToAnsiStyle Keyword   = Terminal.italicized <> Terminal.colorDull Terminal.Green
annToAnsiStyle Literal   = Terminal.colorDull Terminal.Magenta
annToAnsiStyle Reference = Terminal.colorDull Terminal.Blue
annToAnsiStyle Syntax    = Terminal.bold <> Terminal.colorDull Terminal.Cyan

ground, keyword, literal, reference, syntax :: Doc Ann -> Doc Ann
ground    = Pretty.annotate Ground
keyword   = Pretty.annotate Keyword
literal   = Pretty.annotate Literal
reference = Pretty.annotate Reference
syntax    = Pretty.annotate Syntax

comma, langle, rangle, lbrace, rbrace, lbracket, rbracket, lparen, rparen, dot :: Doc Ann
comma    = syntax Pretty.comma
langle   = syntax Pretty.langle
rangle   = syntax Pretty.rangle
lbrace   = syntax Pretty.lbrace
rbrace   = syntax Pretty.rbrace
lbracket = syntax Pretty.lbracket
rbracket = syntax Pretty.rbracket
lparen   = syntax Pretty.lparen
rparen   = syntax Pretty.rparen
dot      = syntax "."

angles, braces, brackets, parens :: Doc Ann -> Doc Ann
angles   = Pretty.enclose langle rangle
braces   = Pretty.enclose lbrace rbrace
brackets = Pretty.enclose lbracket rbracket
parens   = Pretty.enclose lparen rparen

prettyTy :: forall (t :: Ty). Sing t -> Doc Ann
prettyTy (STuple3 t n _) =
  let ty = keyword $ case t of
        SClock    -> "Clock"
        SSigned   -> "SInt"
        SUnsigned -> "UInt"
      width = angles $ Pretty.pretty $ nat (fromSing n)
   in ty <> case t of
              SClock -> mempty
              _      -> width
