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
      , PatternSynonyms
      , ScopedTypeVariables
      , TemplateHaskell
      , TypeFamilies
      , TypeOperators
      , UndecidableInstances #-}
module Firrtl.Lo.TypeCheck.Ty where

import Data.Singletons.TH
import Data.Nat
import Data.Type.Bool
import Numeric.Natural (Natural)

$(singletons [d|
  data Gender = Bi | Female | Male
    deriving (Eq, Show)

  data TyRtl = Clock | Signed | Unsigned
    deriving (Eq, Show)

  type Ty  = (TyRtl, Nat, Gender)

  |])

nat :: Nat -> Natural
nat Z = 0
nat (S n) = 1 + nat n

tyNat :: Ty -> (TyRtl, Natural, Gender)
tyNat (t, n, g) = (t, nat n, g)

pattern SOne = SS SZ
pattern STwo = SS SOne
pattern SFour = SS (SS STwo)

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

type family IsBi (a :: Gender) where
  IsBi 'Bi = 'True
  IsBi _   = 'False

type family IsFemale (a :: Gender) where
  IsFemale 'Female = 'True
  IsFemale _       = 'False

type family IsMale (a :: Gender) where
  IsMale 'Male = 'True
  IsMale _     = 'False

type family IsValidLHS (a :: Gender) where
  IsValidLHS a = IsBi a || IsFemale a

type family IsValidRHS (a :: Gender) where
  IsValidRHS a = IsBi a || IsMale a
