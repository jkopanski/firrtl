{-|
This module is highly experimental.

The goal is to create representation
that will allow to create only valid
(typesafe) LoFIRRTL expressions.

This will be the output of the typechecking phase.
Ready to compile/interpret whatever.

Basically combination of articles:
http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html
https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html
https://blog.jle.im/entry/introduction-to-singletons-1.html
|-}
{-# language
        DataKinds
      , EmptyCase
      , FlexibleInstances
      , GADTs
      , InstanceSigs
      , PolyKinds
      , ScopedTypeVariables
      , TemplateHaskell
      , TypeApplications
      , TypeFamilies
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

  data Signedness = Unsigned | Signed
    deriving (Eq, Show)
  |])

data Ty :: Signedness -> Nat -> Gender -> Type where
  TyRtl :: forall (s :: Signedness) (n :: Nat) (g :: Gender). Signedness -> Nat -> Gender -> Ty s n g

deriving instance Eq (Ty s n g)

data Expr :: Ty s n g -> Type where
  Const :: Int -> Expr ('TyRtl s n g)
  Ref   :: String -> Expr ('TyRtl s n g)
  Valid
    :: Expr ('TyRtl 'Unsigned 1 'Male)
    -> Expr ('TyRtl s n g)
    -> Expr ('TyRtl s n g)

deriving instance Show (Expr t)

sign_ :: Sing s -> Expr ('TyRtl s n g) -> Signedness
sign_ SSigned   _ = Signed
sign_ SUnsigned _ = Unsigned

sign :: SingI s => Expr ('TyRtl s n g) -> Signedness
sign = sign_ sing

width_ :: Sing n -> Expr ('TyRtl s n g) -> Natural
width_ s _ = fromSing s

width :: SingI n => Expr ('TyRtl s n g) -> Natural
width = width_ sing

gender_ :: Sing g -> Expr ('TyRtl s n g) -> Gender
gender_ SBi     _ = Bi
gender_ SFemale _ = Female
gender_ SMale   _ = Male

gender :: SingI g => Expr ('TyRtl s n g) -> Gender
gender = gender_ sing
