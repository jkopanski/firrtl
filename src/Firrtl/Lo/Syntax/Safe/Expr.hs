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
        EmptyCase
      , PolyKinds
      , ScopedTypeVariables
      , TypeInType
      , UndecidableInstances #-}
module Firrtl.Lo.Syntax.Safe.Expr
  ( ExprF (..)
  , SomeExpr (..)
  , TFix (..)
  , isCond
  , typeOf
  , tyrtl
  , width
  , gender

  -- Fix'd Expr
  , Expr
  , fromExpr
  , fromExpr'
  , mkUInt
  , mkSInt
  , mkRef
  , mkValid
  , mkMux

  -- Traversals
  , TFunctor (..)
  , I (..)
  , K (..)
  , hcata
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
  --   These are 2 separate constructors, to force UInt value to be a Natural.
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

data SomeExpr :: * where
  MkSomeExpr :: Sing t -> Expr t -> SomeExpr

mkUInt :: forall (t :: Ty) (n :: Nat)
       .  t ~ '( 'Unsigned, n, 'Male)
       => Sing t -> Natural -> Expr t
mkUInt s n = TFix (UInt s n)

mkSInt :: forall (t :: Ty) (n :: Nat)
       .  t ~ '( 'Signed, n, 'Male)
       => Sing t -> Int -> Expr t
mkSInt s i = TFix (SInt s i)

mkRef :: Sing t -> Id -> Expr t
mkRef s i = TFix (Ref s i)

mkValid :: Sing t -> Expr '( 'Unsigned, Lit 1, 'Male) -> Expr t -> Expr t
mkValid s c v = TFix (Valid s c v)

mkMux :: Sing t -> Expr '( 'Unsigned, Lit 1, 'Male) -> Expr t -> Expr t -> Expr t
mkMux s c l r = TFix (Mux s c l r)

fromExpr :: ExprF (TFix ExprF) t -> SomeExpr
fromExpr e = case e of
  UInt s _ -> MkSomeExpr s (TFix e)
  SInt s _ -> MkSomeExpr s (TFix e)
  Ref  s _ -> MkSomeExpr s (TFix e)
  Valid s _ _ -> MkSomeExpr s (TFix e)
  Mux s _ _ _ -> MkSomeExpr s (TFix e)

fromExpr' :: SingI t => Expr t -> SomeExpr
fromExpr' = MkSomeExpr sing

newtype TFix (h :: (Ty -> *) -> Ty -> *) (t :: Ty) =
  TFix { unTFix :: h (TFix h) t }
type Expr = TFix ExprF

type f :~> g = forall a. f a -> g a

class TFunctor (h :: (Ty -> *) -> Ty -> *) where
  tfmap :: (e :~> f) -> h e :~> h f

instance TFunctor ExprF where
  tfmap _ (UInt s n) = UInt s n
  tfmap _ (SInt s i) = SInt s i
  tfmap _ (Ref  s i) = Ref  s i
  tfmap f (Valid s cond sig) = Valid s (f cond) (f sig)
  tfmap f (Mux   s cond a b) = Mux s (f cond) (f a) (f b)

hcata :: TFunctor h => (h f :~> f) -> TFix h :~> f
hcata alg = alg . tfmap (hcata alg) . unTFix

-- Const and Identity functors for Ty kind
newtype K x (t :: Ty) = K { unK :: x }
newtype I x = I { unI :: x }
 
isCond :: SomeExpr -> Bool
isCond expr = case typeOf expr of
  (Unsigned, 1, Male) -> True
  _ -> False

typeOf :: SomeExpr -> (TyRtl, Natural, Gender)
typeOf (MkSomeExpr s _) = tyNat $ fromSing s

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
