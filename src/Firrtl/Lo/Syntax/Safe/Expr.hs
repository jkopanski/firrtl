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
      , KindSignatures
      , PolyKinds
      , ScopedTypeVariables
      , TypeFamilies
      , TypeOperators
      , TypeInType
      , UndecidableInstances #-}
module Firrtl.Lo.Syntax.Safe.Expr
  ( ExprF (..)
  , SomeExpr (..)
  , TFix (..)
  , fromExpr_
  , fromExpr
  , isCond
  , typeOf
  , tyrtl
  , width
  , gender
  -- , module Firrtl.Lo.Syntax.Safe.Types

  -- Fix'd Expr
  , Expr
  , mkUInt
  , mkSInt
  , mkRef
  , mkValid
  , mkMux
  ) where

import Data.Kind (type (*))
import Data.Singletons
import Data.Singletons.TH
import Data.Nat

import Numeric.Natural (Natural)

import Firrtl.Lo.Syntax.Common  (Id)
import Firrtl.Lo.Syntax.Safe.PrimOpsTy
import Firrtl.Lo.TypeCheck.Ty

data ExprF :: (Ty -> *) -> Ty -> * where
  -- | Constants
  UInt :: forall (s :: TyRtl) (n :: Nat) (g :: Gender) (t :: Ty) (r :: Ty -> *)
       .  ( t ~ '(s, n, g)
          , s ~ 'Unsigned
          , g ~ 'Male
          )
       => Sing t -> Natural -> ExprF r t

  SInt :: forall (s :: TyRtl) (n :: Nat) (g :: Gender) (t :: Ty) (r :: Ty -> *)
       .  ( t ~ '(s, n, g)
          , s ~ 'Signed
          , g ~ 'Male
          )
       => Sing t -> Int     -> ExprF r t

  Ref  :: forall (t :: Ty) (r :: Ty -> *)
       .  Sing t -> Id      -> ExprF r t

  -- | Standard expressions
  Valid :: forall (t :: Ty) (r :: Ty -> *)
        .  Sing t -> r '( 'Unsigned, Lit 1, 'Male) -> r t        -> ExprF r t

  Mux   :: forall (t :: Ty) (r :: Ty -> *)
        .  Sing t -> r '( 'Unsigned, Lit 1, 'Male) -> r t -> r t -> ExprF r t

  -- | PrimOps with some complex type expr
  Add :: forall (s1 :: TyRtl) (w1 :: Nat) (s2 :: TyRtl) (w2 :: Nat) (r :: Ty -> *)
      .  (NotClock s1 && NotClock s2) ~ 'True
      => r '(s1, w1, 'Male)
      -> r '(s2, w2, 'Male)
      -> ExprF r (AddTy s1 w1 s2 w2)

-- h :: (Ty s n g -> *) -> Ty s n g -> *
-- t :: Ty s n g
newtype TFix (h :: (Ty -> *) -> Ty -> *) (t :: Ty) =
  TFix { unTFix :: h (TFix h) t }
type Expr = TFix ExprF

type e :~> f = forall (t :: Ty). e t -> f t

data SomeExpr :: * where
  MkSomeExpr :: Sing t -> Expr t -> SomeExpr

mkUInt :: forall (t :: Ty) (n :: Nat)
       .  t ~ '( 'Unsigned, n, 'Male)
       => Sing t -> Natural -> Expr t
mkUInt s n = withSingI s (TFix $ UInt n) 

mkSInt :: forall (t :: Ty) (n :: Nat)
       .  t ~ '( 'Signed, n, 'Male)
       => Sing t -> Int -> Expr t
mkSInt s n = withSingI s (TFix $ SInt n)

mkRef :: Sing t -> Id -> Expr t
mkRef s r = withSingI s (TFix $ Ref r)

mkValid :: Sing t -> Expr '( 'Unsigned, Lit 1, 'Male) -> Expr t -> Expr t
mkValid s c v = withSingI s (TFix $ Valid c v)

mkMux :: Sing t -> Expr '( 'Unsigned, Lit 1, 'Male) -> Expr t -> Expr t -> Expr t
mkMux s c l r = withSingI s (TFix $ Mux c l r)

fromExpr_ :: Sing t -> Expr t -> SomeExpr
fromExpr_ = MkSomeExpr

fromExpr :: SingI t => Expr t -> SomeExpr
fromExpr = MkSomeExpr sing

fromExpr' :: Expr t -> SomeExpr
fromExpr' e = _ e

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

isCond :: SomeExpr -> Bool
isCond expr = case typeOf expr of
  (Unsigned, 1, Male) -> True
  _ -> False

-- withExpr :: Ty -> (forall t. Sing t -> Expr t -> r) -> r
-- withExpr 

typeOf :: SomeExpr -> (TyRtl, Natural, Gender)
typeOf (MkSomeExpr s e) = tyNat $ fromSing s

tyrtl_ :: Sing s -> ExprF r '(s, n, g) -> TyRtl
tyrtl_ SSigned   _ = Signed
tyrtl_ SUnsigned _ = Unsigned
tyrtl_ SClock    _ = Clock

tyrtl :: SingI s => ExprF r '(s, n, g) -> TyRtl
tyrtl = tyrtl_ sing

width_ :: Sing n -> ExprF r '(s, n, g) -> Natural
width_ s _ = nat $ fromSing s

width :: SingI n => ExprF r '(s, n, g) -> Natural
width = width_ sing

gender_ :: Sing g -> ExprF r '(s, n, g) -> Gender
gender_ SBi     _ = Bi
gender_ SFemale _ = Female
gender_ SMale   _ = Male

gender :: SingI g => ExprF r '(s, n, g) -> Gender
gender = gender_ sing
