{-# language
        DataKinds
      , EmptyCase
      , ScopedTypeVariables
      , TemplateHaskell
      , TypeFamilies
      , UndecidableInstances #-}
module Firrtl.Lo.Syntax.Safe.Types where

import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Type.Bool

$(singletons [d|
  data Gender = Bi | Female | Male
    deriving (Eq, Show)

  data TyRtl = Unsigned | Signed | Clock
    deriving (Eq, Show)
  |])

type Ty = (TyRtl, Nat, Gender)

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
