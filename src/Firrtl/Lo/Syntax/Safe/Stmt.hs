{-# language
        DataKinds
      , GADTs
      , KindSignatures
      , TypeOperators #-}
module Firrtl.Lo.Syntax.Safe.Stmt where

import Data.Singletons.Prelude (type (>=))
import Data.Width

import Firrtl.Lo.Syntax.Common (Id)
import Firrtl.Lo.Syntax.Safe.Expr
import Firrtl.Lo.TypeCheck.Ty

data Stmt :: * where
  Block   :: [Stmt] -> Stmt
  Connect :: forall (s :: TyRtl) (ln :: BW) (rn :: BW) (lg :: Gender) (rg :: Gender)
           . ( (ln >= rn) ~ 'True
             , IsValidLHS lg ~ 'True
             , IsValidRHS rg ~ 'True
             )
          => Expr '(s, ln, lg) -> Expr '(s, rn, rg) -> Stmt
  Empty   :: Stmt
  Node    :: Id -> Expr '(s, n, 'Male) -> Stmt
