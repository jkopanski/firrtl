{-# language
        DataKinds
      , GADTs
      , KindSignatures
      , TypeOperators #-}
module Firrtl.Lo.Syntax.Safe.Stmt where

import Data.Nat
import Data.Singletons.Prelude (type (>=))

import Firrtl.Lo.Syntax.Common (Id)
import Firrtl.Lo.Syntax.Safe.Expr
import Firrtl.Lo.TypeCheck.Ty

data Stmt :: * where
  Block   :: [Stmt] -> Stmt
  Connect :: forall (s :: TyRtl) (ln :: Nat) (rn :: Nat) (lg :: Gender) (rg :: Gender)
           . ( (ln >= rn) ~ 'True
             , IsValidLHS lg ~ 'True
             , IsValidRHS rg ~ 'True
             )
          => Expr '(s, ln, lg) -> Expr '(s, rn, rg) -> Stmt
  Empty   :: Stmt
  Node    :: Id -> Expr '(s, n, 'Male) -> Stmt
