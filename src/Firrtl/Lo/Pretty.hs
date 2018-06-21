module Firrtl.Lo.Pretty where

import Data.Functor.Foldable (cata)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Prettyprint.Doc (Doc, Pretty, space)
import Formatting.Buildable (Buildable(..))
import Numeric.Natural (Natural)

import qualified Data.Text.Lazy                            as Text
import qualified Data.Text.Lazy.Builder                    as Builder
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal
import qualified Firrtl.Lo.Syntax.Expr                     as Expr
import qualified Firrtl.Lo.TypeCheck.Types                 as Types

data Ann
  = Ground
  | Keyword
  | Operator

pretty :: Pretty a => a -> Text
pretty = Pretty.renderLazy
       . Pretty.layoutPretty Pretty.defaultLayoutOptions
       . Pretty.pretty

ground, keyword, operator :: Doc Ann -> Doc Ann
ground   = Pretty.annotate Ground
keyword  = Pretty.annotate Keyword
operator = Pretty.annotate Operator

annToAnsiStyle :: Ann -> Terminal.AnsiStyle
annToAnsiStyle Ground   = Terminal.bold <> Terminal.colorDull Terminal.Green
annToAnsiStyle Keyword  = Terminal.bold <> Terminal.colorDull Terminal.Green
annToAnsiStyle operator = Terminal.bold <> Terminal.colorDull Terminal.Green

instance Pretty.Pretty Expr.Immediate where
  pretty (Expr.Imm nat) = Pretty.pretty nat

instance Pretty.Pretty Expr.UnaryOp where
  pretty Expr.AndR       = Pretty.pretty ("andr" :: Text)
  pretty Expr.AsClock    = Pretty.pretty ("asClock" :: Text)
  pretty Expr.AsSigned   = Pretty.pretty ("asSInt" :: Text)
  pretty Expr.AsUnsigned = Pretty.pretty ("asUInt" :: Text)
  pretty Expr.Cvt        = Pretty.pretty ("cvt" :: Text)
  pretty Expr.Neg        = Pretty.pretty ("neg" :: Text)
  pretty Expr.Not        = Pretty.pretty ("not" :: Text)
  pretty Expr.OrR        = Pretty.pretty ("orr" :: Text)
  pretty Expr.XorR       = Pretty.pretty ("xorr" :: Text)

instance Pretty.Pretty Expr.BinaryOp where
  pretty Expr.Add  = Pretty.pretty ("add" :: Text)
  pretty Expr.And  = Pretty.pretty ("and" :: Text)
  pretty Expr.Cat  = Pretty.pretty ("cat" :: Text)
  pretty Expr.Div  = Pretty.pretty ("div" :: Text)
  pretty Expr.DShl = Pretty.pretty ("dshl" :: Text)
  pretty Expr.DShr = Pretty.pretty ("dshr" :: Text)
  pretty Expr.Eq   = Pretty.pretty ("eq" :: Text)
  pretty Expr.Geq  = Pretty.pretty ("geq" :: Text)
  pretty Expr.Gt   = Pretty.pretty ("gt" :: Text)
  pretty Expr.Leq  = Pretty.pretty ("leq" :: Text)
  pretty Expr.Lt   = Pretty.pretty ("lt" :: Text)
  pretty Expr.Mod  = Pretty.pretty ("mod" :: Text)
  pretty Expr.Mul  = Pretty.pretty ("mul" :: Text)
  pretty Expr.Neq  = Pretty.pretty ("neq" :: Text)
  pretty Expr.Or   = Pretty.pretty ("or" :: Text)
  pretty Expr.Sub  = Pretty.pretty ("sub" :: Text)
  pretty Expr.Xor  = Pretty.pretty ("xor" :: Text)
  pretty Expr.Head = Pretty.pretty ("head" :: Text)
  pretty Expr.Pad  = Pretty.pretty ("pad" :: Text)
  pretty Expr.Shl  = Pretty.pretty ("shl" :: Text)
  pretty Expr.Shr  = Pretty.pretty ("shr" :: Text)
  pretty Expr.Tail = Pretty.pretty ("tail" :: Text)

instance Pretty.Pretty Expr.TernaryOp where
  pretty Expr.Bits = Pretty.pretty ("bits" :: Text)

instance Pretty.Pretty Expr.Literal where
  pretty (Expr.UInt _ u) = Pretty.pretty ("UInt" :: Text)
                      <> Pretty.parens (Pretty.pretty u)
  pretty (Expr.SInt _ s) = Pretty.pretty ("SInt" :: Text)
                      <> Pretty.parens (Pretty.pretty s)

prettyType :: Types.Type -> Doc Ann
prettyType (Types.Natural n)  = Pretty.pretty n
prettyType (Types.Unsigned n) = ground "UInt" <> Pretty.angles (Pretty.pretty n)
prettyType (Types.Signed n)   = ground "SInt" <> Pretty.angles (Pretty.pretty n)
prettyType  Types.Clock       = ground "Clock"

prettyExpr :: Expr.Expr -> Doc Ann
prettyExpr = cata alg
  where alg :: Expr.ExprF (Doc Ann) -> Doc Ann
        alg (Expr.RefF ident)        = Pretty.pretty ident
        alg (Expr.ParameterF imm)    = Pretty.pretty imm
        alg (Expr.LitF lit)          = Pretty.pretty lit
        alg (Expr.ValidF cond sig)   = keyword "validif"
                                    <> Pretty.tupled [cond, sig]
        alg (Expr.MuxF cond l r)     = keyword "mux"
                                    <> Pretty.tupled [cond, l, r]
        alg (Expr.UnaryF op a)       = operator $ Pretty.pretty op
                                    <> Pretty.tupled [a]
        alg (Expr.BinaryF op a b)    = operator $ Pretty.pretty op
                                    <> Pretty.tupled [a, b]
        alg (Expr.TernaryF op a b c) = operator $ Pretty.pretty op
                                    <> Pretty.tupled [a, b, c]
