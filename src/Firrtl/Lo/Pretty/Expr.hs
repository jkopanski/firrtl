{-# language
        DataKinds
      , TypeInType #-}
module Firrtl.Lo.Pretty.Expr where

import           Data.Kind (type (*))
import           Data.Nat
import           Data.Singletons
import           Data.Singletons.Prelude.Tuple
import           Data.Text.Prettyprint.Doc (Doc)
import qualified Data.Text.Prettyprint.Doc as Pretty

import Firrtl.Lo.Pretty.Common
import Firrtl.Lo.Syntax.Safe.Expr
import Firrtl.Lo.TypeCheck.Ty (Gender (..), Ty, TyRtl (..), nat)

prettyExpr :: forall (r :: Ty -> *) (t :: TyRtl) (n :: Nat) (g :: Gender)
           .  ExprF r '(t, n, g) -> Doc Ann
prettyExpr (UInt (STuple3 _ n _) u) =
  keyword "UInt" <> angles (Pretty.pretty (nat (fromSing n)))
                 <> parens (literal $ Pretty.pretty u)

prettyExpr (SInt (STuple3 _ n _) u) =
  keyword "SInt" <> angles (Pretty.pretty (nat (fromSing n)))
                 <> parens (literal $ Pretty.pretty u)

