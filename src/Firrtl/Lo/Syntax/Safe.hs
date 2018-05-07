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
      , TypeOperators
      , TypeInType
      , UndecidableInstances #-}
module Firrtl.Lo.Syntax.Safe where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Kind
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.TH
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

data Ty :: Signedness -> Nat -> Gender -> Type where
  TyRtl :: forall (s :: Signedness) (n :: Nat) (g :: Gender). Signedness -> Nat -> Gender -> Ty s n g

deriving instance Eq (Ty s n g)

data ExprF :: (Ty s n g -> Type) -> Ty s n g -> Type where
  Lit
    :: Int
    -> ExprF r ('TyRtl s n g)
  Ref
    :: String
    -> ExprF r ('TyRtl s n g)
  Valid
    :: r ('TyRtl 'Unsigned 1 'Male)
    -> r ('TyRtl s n g)
    -> ExprF r ('TyRtl s n g)

newtype HFix h a = HFix { unFix :: h (HFix h) a }
type Expr = HFix ExprF

newtype I a = I { unI :: a }
newtype K a t = K { unK :: a }

instance Functor (K a) where
  fmap _ (K v) = K v

type f :~> g = forall t. f t -> g t

class HFunctor (h :: (Ty s n g -> Type) -> Ty s n g -> Type) where
  hfmap :: (f :~> e) -> h f :~> h e

instance HFunctor ExprF where
  hfmap _ (Lit i) = Lit i
  hfmap _ (Ref i) = Ref i
  hfmap f (Valid cond sig) = Valid (f cond) (f sig)

hcata :: HFunctor h => (h f :~> f) -> HFix h :~> f
hcata alg = alg . hfmap (hcata alg) . unFix

prettyExpr :: Expr ('TyRtl s n g) -> Doc Ann
prettyExpr = unK . hcata alg
  where alg :: ExprF (K (Doc Ann)) :~> K (Doc Ann)
        alg (Ref ident)      = K $ Pretty.pretty ident
        alg (Lit lit)        = K $ Pretty.pretty lit
        alg (Valid cond sig)
          = (\x y z ->
               Pretty.pretty "validif" <> Pretty.tupled [y, z]) <$> cond <*> sig

-- sig  :: K (Doc Ann) ('TyRtl s3 n3 g3)
-- cond :: K (Doc Ann) ('TyRtl 'Unsigned 1 'Male)
-- alg  :: ExprF (K (Doc Ann)) t -> K (Doc Ann) t

data Ann
  = Ground
  | Keyword
  | Operator

-- deriving instance Show (ExprF t)

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
