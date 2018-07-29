{- language
       DataKinds
     , TypeInType #-}
module Firrtl.Lo.Pretty.Stmt
  ( pretty
  , prettyBlock
  ) where

import           Data.Text.Prettyprint.Doc (Doc, (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty

import           Firrtl.Lo.Pretty.Common
import qualified Firrtl.Lo.Pretty.Expr      as PEx
import           Firrtl.Lo.Syntax.Safe.Stmt

pretty :: Stmt -> Doc Ann
pretty (Block stmts) = Pretty.group $ prettyBlock $ (pretty <$> stmts)

pretty (Connect lhs rhs) =
  PEx.pretty lhs <+> syntax "<="
                 <+> PEx.pretty rhs  

pretty Empty = keyword "skip"

pretty (Node ident expr) =
  keyword "node" <+> reference (Pretty.pretty ident)
                 <+> syntax "="
                 <+> PEx.pretty expr

prettyBlock :: [Doc Ann] -> Doc Ann
prettyBlock = 
  let open  = Pretty.flatAlt "" "( "
      close = Pretty.flatAlt "" " )"
      separator = Pretty.flatAlt "" ", "
   in Pretty.align . Pretty.encloseSep open close separator
