{-|
This module is highly experimental.

The goal is to create representation
that will allow to create only valid
(typesafe) LoFIRRTL expressions.

This will be the output of the typechecking phase.
Ready to compile/interpret whatever.
|-}
{-# language
        DataKinds
      , EmptyCase
      , GADTs
      , InstanceSigs
      , PolyKinds
      , ScopedTypeVariables
      , TemplateHaskell
      , TypeApplications
      , TypeInType
      , UndecidableInstances #-}
module Firrtl.Lo.Syntax.Safe where

import Data.Kind
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.TH
import Numeric.Natural

$(singletons [d|
  data Gender = Bi | Female | Male
    deriving (Eq, Show)
  |])

$(singletons [d|
  data Signedness = Unsigned | Signed
    deriving (Eq, Show)
  |])

-- $(singletons [d|
--   data Ty = Ty (Signedness, Nat, Gender)
--     deriving Eq
--   |])

-- deriving instance Show Nat => Show Ty

data TyRtl (s :: Signedness) (n :: Nat) (g :: Gender)
  = TyRtl Signedness Nat Gender
  deriving Eq

deriving instance Show Nat => Show (TyRtl s n g)

data ExprF :: Signedness -> Nat -> Gender -> Type where
  Const :: Int    -> ExprF s n 'Male
  Ref   :: String -> ExprF s n g
  Valid :: ExprF 'Unsigned 1 'Male -> ExprF s n g

sign_ :: Sing s -> ExprF s n g -> Signedness
sign_ SSigned   _ = Signed
sign_ SUnsigned _ = Unsigned

sign :: SingI s => ExprF s n g -> Signedness
sign = sign_ sing

gender_ :: Sing g -> ExprF s n g -> Gender
gender_ SBi     _ = Bi
gender_ SFemale _ = Female
gender_ SMale   _ = Male

gender :: SingI g => ExprF s n g -> Gender
gender = gender_ sing

width_ :: Sing n -> ExprF s n g -> Natural
width_ s _ = fromSing s

width :: SingI n => ExprF s n g -> Natural
width = width_ sing
