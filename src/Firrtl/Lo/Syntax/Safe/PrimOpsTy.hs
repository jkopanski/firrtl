{-# language
        DataKinds
      , TypeFamilies
      , TypeOperators
      , UndecidableInstances #-}
module Firrtl.Lo.Syntax.Safe.PrimOpsTy where

import Data.Singletons.Prelude (type (+), type (-), Max)
import Data.Singletons.TypeLits
-- import Data.Nat

import Firrtl.Lo.TypeCheck.Ty

type family AddTy (s1 :: TyRtl) (w1 :: Nat) (s2 :: TyRtl) (w2 :: Nat) where
  AddTy 'Unsigned w1 'Unsigned w2 = '( 'Unsigned, 1 + Max w1 w2, 'Male)
  AddTy 'Unsigned w1 'Signed   w2 = '( 'Signed,   2 + Max w1 (w2 - 1), 'Male)
  AddTy 'Signed   w1 'Unsigned w2 = '( 'Signed,   2 + Max (w1 - 1) w2, 'Male)
  AddTy 'Signed   w1 'Signed   w2 = '( 'Signed,   1 + Max w1 w2, 'Male)
-- AddTy _         _  _         _  = Void

