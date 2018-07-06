{-# language
        DataKinds
      , GADTs
      , KindSignatures #-}
module Firrtl.Lo.Syntax.Safe.Stmt where

import Firrtl.Lo.Syntax.Common (Id)
import Firrtl.Lo.Syntax.Safe.Expr
import Firrtl.Lo.TypeCheck.Ty

data Stmt :: * where
  Block :: [Stmt] -> Stmt
  Empty :: Stmt
  Node  :: Id -> Expr '(s, n, 'Male) -> Stmt
