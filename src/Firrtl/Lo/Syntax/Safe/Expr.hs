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
  , Functor
  ) where

import Prelude hiding (Functor (fmap), id, (.))
import Control.Category (Category (id, (.)))

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

-- fromExpr'' :: Sing t -> Expr t -> SomeExpr
-- fromExpr'' = MkSomeExpr

newtype TFix (h :: (Ty -> *) -> Ty -> *) (t :: Ty) =
  TFix { unTFix :: h (TFix h) t }
type Expr = TFix ExprF

-- instance Category Ty where
--   id = Prelude.id
--   (.) = (Prelude..)

-- newtype TyFunction (a :: Ty) (b :: Ty) =
--   TyFunction { tyfun :: (Ty, Nat, Gender) -> (Ty, Nat, Gender) }

-- instance Category TyFunction where
--   id = TyFunction Prelude.id
--   (.) a b = TyFunction ((Prelude..) (tyfun a) (tyfun b))

-- instance Functor f TyFunction (->) where
--   fmap = undefined

-- infixr 0 ~~>
-- type f ~~> g = forall (t :: Ty). f t -> g t

-- infixr 0 :~>, $$
-- newtype f :~> g = NT { ($$) :: f ~~> g }

-- instance Category (:~>) where
--   id = NT id
--   NT f . NT g = NT (f . g)

-- instance Functor ExprF (:~>) (->) where
--   fmap = undefined

-- type e :~> f = forall t {- (t :: Ty) -} . e t -> f t

-- class HFunctor (h :: (* -> *) -> * -> *) where
--   hfmap :: (f :~> g) -> h f :~> h g

-- instance HFunctor ExprF where
--   hfmap f (UInt s n) = UInt s n


-- Kinder functor formulation
-- ripped from https://github.com/rampion/kinder-functor
type family Cat k :: k -> k -> *

class (Category (Cat j), Category (Cat k)) => Functor (f :: j -> k) where
  fmap :: Cat j a b -> Cat k (f a) (f b)

type instance Cat (*) = (->)

infixr 0 $$

newtype NT (cat :: k -> k -> *) (f :: j -> k) (g :: j -> k) =
  NT { ($$) :: forall x. cat (f x) (g x) }

instance Category cat => Category (NT cat) where
  id = NT id
  NT a . NT b = NT (a . b) 

type instance Cat (j -> k) = NT (Cat k)

data CTy (a :: Ty) (b :: Ty) where
  CId :: CTy a a
  -- CF  :: CTy a b -> a -> b -> CTy a b
  CC  :: forall (a :: Ty) (b :: Ty) (c :: Ty). CTy b c -> CTy a b -> CTy a c

type instance Cat Ty = CTy

instance Category CTy where
  id = CId
  (.) = CC

instance Functor (CTy a) where
  fmap = (.)

instance (Category cat, Cat k ~ cat) => Functor (NT cat f) where
  fmap = (.)

instance Functor NT where
  fmap h = NT $ NT $ \(NT h') -> NT $ (($$) h) $$ h'

instance Functor ExprF where
  fmap (NT h) = NT $ \ex -> case ex of
    UInt s n -> UInt s n
    SInt s i -> SInt s i
    Ref  s i -> Ref  s i
    Valid s cond a -> Valid s (h cond) (h a)
    Mux s cond a b -> Mux s (h cond) (h a) (h b)
    -- Valid s cond a -> Valid (( fmap h ) cond) (( fmap h ) a) -- fmap h _  -- (fmap a)
    -- -- where g :: ExprF a t -> ExprF b t
         --  UInt (STuple3 SUnsigned SZ SMale) 1
  

--   fmap CId t = t
--   fmap CC a = CC . a

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
