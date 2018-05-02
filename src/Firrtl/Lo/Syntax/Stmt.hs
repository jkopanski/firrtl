{-|
Statement definition for FIRRTL.

Lowered FIRRTL doesn't have partial connect
and conditional statements.
|-}
module Firrtl.Lo.Syntax.Stmt
  ( Stmt (..)
  ) where

import Data.Text (Text)

import Firrtl.Lo.Syntax.Common
import Firrtl.Lo.Syntax.Expr
import Firrtl.Lo.TypeCheck.Types

data Stmt
  = Block [Stmt]
  -- | Cond Expr Stmt Stmt
  | Connect Expr Expr
  | Empty
  -- | Instance Id Ident
  | Invalid Expr
  -- | Memory Mem
  | Node Id Expr
  -- | Partial Expr Expr
  | Print Expr Expr Text [Expr]
  -- | Reg Id Type (Maybe Expr) (Maybe Reset)
  | Stop Expr Expr Int
  | Wire Id Type
  deriving (Eq, Show)
