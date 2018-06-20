{-|
Firrtl type definitions,
with singleton defs and various type synonyms.

Type is a triple, cointaining:
- 'real' type:
  - clock,
  - signed integer
  - unsigned integer
- bit width
- gender (or direction)
|-}
{-# language
        DataKinds
      , EmptyCase
      , ScopedTypeVariables
      , TemplateHaskell
      , TypeFamilies
      , UndecidableInstances #-}
module Firrtl.Lo.TypeCheck.Ty where

import Data.Singletons.TH
import Data.Singletons.TypeLits
-- import Data.Nat
import Data.Type.Bool
import Numeric.Natural

$(singletons [d|
  data Gender = Bi | Female | Male
    deriving (Eq, Show)

  data TyRtl = Unsigned | Signed | Clock
    deriving (Eq, Show)

  type Ty  = (TyRtl, Natural, Gender)

  |])

type KTy = (TyRtl, Nat, Gender)

type family IsClock (a :: TyRtl) where
  IsClock 'Clock = 'True
  IsClock _      = 'False

type family NotClock (a :: TyRtl) where
  NotClock a = Not (IsClock a)

type family IsSigned (a :: TyRtl) where
  IsSigned 'Signed = 'True
  IsSigned _       = 'False

type family IsUnsigned (a :: TyRtl) where
  IsUnsigned 'Unsigned = 'True
  IsUnsigned _         = 'False
