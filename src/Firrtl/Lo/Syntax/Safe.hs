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
      , KindSignatures
      , GADTs
      , InstanceSigs
      , PolyKinds
      , ScopedTypeVariables
      , TemplateHaskell
      , TypeApplications
      , TypeFamilies
      , TypeOperators
      , TypeInType
      , UndecidableInstances #-}
module Firrtl.Lo.Syntax.Safe where

import Data.Kind (type (*))
import Data.Singletons
import Data.Singletons.Prelude.Num (type (+), type (-))
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Type.Bool

import Data.Monoid     ((<>))
import Data.Text.Lazy  (Text)
import Numeric.Natural (Natural)

import Firrtl.Lo.Syntax.Common  (Id)

-- import Data.Text.Lazy.Builder (Builder)
-- import Data.Text.Prettyprint.Doc (Doc, Pretty, space)
-- import qualified Data.Text.Prettyprint.Doc                 as Pretty
-- import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty
-- import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal

$(singletons [d|
  data Gender = Bi | Female | Male
    deriving (Eq, Show)

  data TyRtl = Unsigned | Signed | Clock
    deriving (Eq, Show)
  |])

type Ty = (TyRtl, Nat, Gender)

-- already defined by singletons prelude
-- instance (SingI Signedness, SingI Nat, SingI Gender) => SingI '(Signedness, Nat, Gender) where
--   sing = STuple3 sing sing sing

-- data Literal :: Ty -> * where
--   UInt :: Natural -> Literal '( 'Unsigned, n, 'Male)
--   SInt :: Int     -> Literal '( 'Signed, n, 'Male)

-- deriving instance Eq (Literal t)
-- deriving instance Show (Literal t)

-- mkUInt :: Sing n -> Natural -> Literal '( 'Unsigned, n, 'Male)
-- mkUInt _ = UInt

-- mkSInt :: Sing n -> Int -> Literal '( 'Signed, n, 'Male)
-- mkSInt _ = SInt

-- data SomeLiteral :: * where
--   MkSomeLiteral :: Sing t -> Literal t -> SomeLiteral

-- data Reference :: Ty -> * where
--   Ref :: Id -> Reference t

-- deriving instance Eq (Reference t)
-- deriving instance Show (Reference t)

-- mkRef :: Sing t -> Id -> Reference t
-- mkRef _ = Ref

-- data SomeReference :: * where
--   MkSomeReference :: Sing t -> Reference t -> SomeReference

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

type family AddTy (s1 :: TyRtl) (w1 :: Nat) (s2 :: TyRtl) (w2 :: Nat) where
  AddTy 'Unsigned w1 'Unsigned w2 = '( 'Unsigned, 1 + Max w1 w2, 'Male)
  AddTy 'Unsigned w1 'Signed   w2 = '( 'Signed,   2 + Max w1 (w2 - 1), 'Male)
  AddTy 'Signed   w1 'Unsigned w2 = '( 'Signed,   2 + Max (w1 - 1) w2, 'Male)
  AddTy 'Signed   w1 'Signed   w2 = '( 'Signed,   1 + Max w1 w2, 'Male)
  -- AddTy _         _  _         _  = Void

data ExprF :: (Ty -> *) -> Ty -> * where
  -- | Constants
  UInt :: Natural -> ExprF r '( 'Unsigned, n, 'Male)
  SInt :: Int     -> ExprF r '( 'Signed,   n, 'Male)
  Ref  :: Id      -> ExprF r t

  -- | Standard expressions
  Valid :: r '( 'Unsigned, 1, 'Male) -> r t -> ExprF r t
  Mux   :: r '( 'Unsigned, 1, 'Male) -> r t -> r t -> ExprF r t

  -- | PrimOps with some complex type expr
  Add :: forall (s1 :: TyRtl) (w1 :: Nat) (s2 :: TyRtl) (w2 :: Nat) (r :: Ty -> *)
      .  ((NotClock s1 && NotClock s2) ~ 'True)
      => r '(s1, w1, 'Male)
      -> r '(s2, w2, 'Male)
      -> ExprF r (AddTy s1 w1 s2 w2)

-- data ExprF :: (Ty s n g -> *) -> Ty s n g -> * where
--   Lit   :: Lit ('TyRtl s n g) -> ExprF r ('TyRtl s n g)
--   Ref   :: Ref ('TyRtl s n g) -> ExprF r ('TyRtl s n g)
--   Valid :: r ('TyRtl 'Unsigned 1 'Male) -> r ('TyRtl s n g) -> ExprF r ('TyRtl s n g)

-- deriving instance Eq (ExprF t)
-- deriving instance Show (ExprF r t)

-- h :: (Ty s n g -> *) -> Ty s n g -> *
-- t :: Ty s n g
-- newtype TFix (h :: (Ty s n g -> *) -> Ty s n g -> *) (t :: Ty s n g) = TFix { unTFix :: h (TFix h) t }
-- type Expr = TFix ExprF

-- type e :~> f = forall (s :: Signedness) (n :: Nat) (g :: Gender). e ('TyRtl s n g) -> f ('TyRtl s n g)

-- class TFunctor (h :: (Ty s n g -> *) -> Ty s n g -> *) where
--   tfmap :: (e :~> f) -> h e :~> h f

-- instance TFunctor ExprF where
--   tfmap _ (Lit l) = Lit l
--   tfmap _ (Ref n) = Ref n
--   tfmap f (Valid cond sig) = Valid (f cond) (f sig)

-- hcata :: TFunctor h => (h f :~> f) -> TFix h :~> f
-- hcata alg = alg . tfmap (hcata alg) . unTFix

-- newtype K x (t :: Ty s n g) = K { unK :: x }

-- prettyExpr :: Expr ('TyRtl s n g) -> Doc Ann
-- prettyExpr = unK . hcata alg

-- alg :: ExprF (K (Doc Ann)) ('TyRtl s n g) -> K (Doc Ann) ('TyRtl s n g)
-- alg (Lit l) = K $ prettyLit l
-- alg (Ref r) = K $ prettyRef r
-- alg (Valid cond sig) =
--   let op   = Pretty.pretty ("validif" :: Text)
--       args = Pretty.parens . Pretty.hsep . Pretty.punctuate Pretty.comma
--    in K $ op <> args [unK cond, unK sig]

-- prettyLit :: Lit ('TyRtl s n g) -> Doc Ann
-- prettyLit l = case l of
--   SInt v -> plit "SInt" <> wid (width $ Lit l) <> val v
--   UInt v -> plit "UInt" <> wid (width $ Lit l) <> val v
--   where
--     plit :: Text -> Doc Ann
--     plit t = Pretty.pretty t
--     val :: Pretty a => a -> Doc Ann
--     val = Pretty.parens . Pretty.pretty
--     wid :: Natural -> Doc Ann
--     wid = Pretty.angles . Pretty.pretty

-- prettyRef :: Ref ('TyRtl s n g) -> Doc Ann
-- prettyRef (Reference name) = Pretty.pretty name

-- data Ann
--   = Ground
--   | Keyword
--   | Operator

-- sign_ :: Sing s -> ExprF r ('TyRtl s n g) -> Signedness
-- sign_ SSigned   _ = Signed
-- sign_ SUnsigned _ = Unsigned

-- sign :: SingI s => ExprF r ('TyRtl s n g) -> Signedness
-- sign = sign_ sing

-- width_ :: Sing n -> ExprF r ('TyRtl s n g) -> Natural
-- width_ s _ = fromSing s

-- width :: SingI n => ExprF r ('TyRtl s n g) -> Natural
-- width = width_ sing

-- gender_ :: Sing g -> ExprF r ('TyRtl s n g) -> Gender
-- gender_ SBi     _ = Bi
-- gender_ SFemale _ = Female
-- gender_ SMale   _ = Male

-- gender :: SingI g => ExprF r ('TyRtl s n g) -> Gender
-- gender = gender_ sing
