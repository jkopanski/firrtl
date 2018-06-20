{-# language
        DataKinds
      , TypeInType #-}
module Firrtl.Lo.TypeCheck.SafeStmt where

import qualified Firrtl.Lo.Syntax.Safe as Safe
import           Firrtl.Lo.Syntax.Expr
import           Firrtl.Lo.TypeCheck.Monad
import           Firrtl.Lo.TypeCheck.Ty
import           Firrtl.Lo.TypeCheck.Types
import           Firrtl.Lo.TypeCheck.Expr

-- instance Typed Stmt where
--   type TypeSafe Stmt = Safe.Expr

