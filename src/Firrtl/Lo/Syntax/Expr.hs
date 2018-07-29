{-# language TemplateHaskell #-}
{-|
Expression definition for FIRRTL.

Lowered FIRRTL doesn't have vectora and bundles,
whus we can omit SubField, SubIndex and SubAccess expressions here.
|-}
module Firrtl.Lo.Syntax.Expr where

import Data.Functor.Foldable.TH
import Numeric.Natural          (Natural)
import Firrtl.Lo.Syntax.Common  (Id)

data UnaryOp
  = AndR | AsClock | AsSigned | AsUnsigned
  | Cvt | Neg | Not | OrR | XorR
  deriving (Eq, Show)

data BinaryOp
  = Add | And | Cat | Div | DShl | DShr | Eq | Geq | Gt
  | Leq | Lt | Mod | Mul | Neq | Or | Sub | Xor
  -- | Parametrized operations
  | Head | Pad | Shl | Shr | Tail
  deriving (Eq, Show)

data TernaryOp = Bits
  deriving (Eq, Show)

-- | Literals are carrying type information from the begining
data Literal
  = UInt (Maybe Natural) Natural
  | SInt (Maybe Natural) Int
  deriving (Eq, Show)

-- | Argument ro paremetrized operators and subindex expression
newtype Immediate = Imm Natural
  deriving (Eq, Show)

data Expr
  = Ref Id
  | Parameter Immediate
  | Lit Literal
  | Valid Expr Expr
  | Mux Expr Expr Expr
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Ternary TernaryOp Expr Expr Expr
  deriving (Eq, Show)

makeBaseFunctor ''Expr
