{-# language
        DataKinds
      , TypeInType #-}
module Firrtl.Lo.Pretty.Expr
  ( pretty ) where

import           Data.Kind (type (*))
import           Data.Singletons
import           Data.Singletons.Prelude.Tuple
import           Data.Text.Prettyprint.Doc (Doc)
import qualified Data.Text.Prettyprint.Doc as Pretty
import           Data.Width

import Firrtl.Lo.Pretty.Common
import Firrtl.Lo.Syntax.Safe.Expr
import Firrtl.Lo.TypeCheck.Ty     (Ty)


pretty :: forall (t :: Ty). Expr t -> Doc Ann
pretty = unK . hcata prettyExprAlg

prettyExprAlg :: forall (t :: Ty). ExprF (K (Doc Ann)) t -> K (Doc Ann) t
prettyExprAlg (UInt (STuple3 _ n _) u) = K $
  keyword "UInt" <> angles (Pretty.pretty (unWidth (fromSing n)))
                 <> parens (literal $ Pretty.pretty u)

prettyExprAlg (SInt (STuple3 _ n _) u) = K $
  keyword "SInt" <> angles (Pretty.pretty (unWidth (fromSing n)))
                 <> parens (literal $ Pretty.pretty u)

prettyExprAlg (Ref s id) = K $ reference $ Pretty.pretty id

prettyExprAlg (Valid s cond signal) = K $
  keyword "validif" <> parens (unK cond <> comma <> unK signal)

prettyExprAlg (Mux s cond a b) = K $
  keyword "mux" <> parens (Pretty.sep $ Pretty.punctuate comma [unK cond, unK a, unK b])
