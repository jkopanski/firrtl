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
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Numeric.Natural

import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Prettyprint.Doc (Doc, Pretty, space)
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal

$(singletons [d|
  data Gender = Bi | Female | Male
    deriving (Eq, Show)

  data Signedness = Unsigned | Signed
    deriving (Eq, Show)
  |])

data Ty :: Signedness -> Nat -> Gender -> * where
  TyRtl :: Signedness -> Nat -> Gender -> Ty s n g

deriving instance Eq (Ty s n g)

data Lit :: Ty s n g -> * where
  SInt :: Int -> Lit ('TyRtl 'Signed n 'Male)
  UInt :: Natural -> Lit ('TyRtl 'Unsigned n 'Male)

deriving instance Eq (Lit t)
deriving instance Show (Lit t)

data Ref :: Ty s n g -> * where
  Reference :: String -> Ref ('TyRtl s n g)

deriving instance Eq (Ref t)
deriving instance Show (Ref t)

data ExprF :: (Ty s n g -> *) -> Ty s n g -> * where
  Lit   :: Lit ('TyRtl s n g) -> ExprF r ('TyRtl s n g)
  Ref   :: Ref ('TyRtl s n g) -> ExprF r ('TyRtl s n g)
  Valid :: r ('TyRtl 'Unsigned 1 'Male) -> r ('TyRtl s n g) -> ExprF r ('TyRtl s n g)

-- deriving instance Eq (ExprF t)
-- deriving instance Show (ExprF r t)

-- h :: (Ty s n g -> *) -> Ty s n g -> *
-- t :: Ty s n g
newtype TFix (h :: (Ty s n g -> *) -> Ty s n g -> *) (t :: Ty s n g) = TFix { unTFix :: h (TFix h) t }
type Expr = TFix ExprF

type e :~> f = forall (s :: Signedness) (n :: Nat) (g :: Gender). e ('TyRtl s n g) -> f ('TyRtl s n g)

class TFunctor (h :: (Ty s n g -> *) -> Ty s n g -> *) where
  tfmap :: (e :~> f) -> h e :~> h f

instance TFunctor ExprF where
  tfmap _ (Lit l) = Lit l
  tfmap _ (Ref n) = Ref n
  tfmap f (Valid cond sig) = Valid (f cond) (f sig)

hcata :: TFunctor h => (h f :~> f) -> TFix h :~> f
hcata alg = alg . tfmap (hcata alg) . unTFix

newtype K x (t :: Ty s n g) = K { unK :: x }

prettyExpr :: Expr ('TyRtl s n g) -> Doc Ann
prettyExpr = unK . hcata alg

alg :: ExprF (K (Doc Ann)) ('TyRtl s n g) -> K (Doc Ann) ('TyRtl s n g)
alg e@(Lit l) = K $ prettyLit l

prettyLit :: Lit ('TyRtl s n g) -> Doc Ann
prettyLit l = case l of
  SInt v -> plit "SInt" <> {- wid (fromSing (sing :: Sing n)) <> -} val v
  UInt v -> plit "UInt" <> {- wid (fromSing (sing :: Sing n)) <> -} val v
  where
    plit :: Text -> Doc Ann
    plit t = Pretty.pretty t
    val :: Pretty a => a -> Doc Ann
    val = Pretty.parens . Pretty.pretty
    wid :: Natural -> Doc Ann
    wid = Pretty.angles . Pretty.pretty

data Ann
  = Ground
  | Keyword
  | Operator

sign_ :: Sing s -> ExprF r ('TyRtl s n g) -> Signedness
sign_ SSigned   _ = Signed
sign_ SUnsigned _ = Unsigned

sign :: SingI s => ExprF r ('TyRtl s n g) -> Signedness
sign = sign_ sing

width_ :: Sing n -> ExprF r ('TyRtl s n g) -> Natural
width_ s _ = fromSing s

width :: SingI n => ExprF r ('TyRtl s n g) -> Natural
width = width_ sing

gender_ :: Sing g -> ExprF r ('TyRtl s n g) -> Gender
gender_ SBi     _ = Bi
gender_ SFemale _ = Female
gender_ SMale   _ = Male

gender :: SingI g => ExprF r ('TyRtl s n g) -> Gender
gender = gender_ sing
