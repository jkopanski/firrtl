{-# language
        DataKinds
      , TypeFamilies
      , TypeOperators
      , UndecidableInstances #-}
module Firrtl.Lo.Syntax.Safe.PrimOpsTy where

import Data.Singletons.Prelude (type (+), type (-), Max)
import Data.Width

import Firrtl.Lo.TypeCheck.Ty

type family AddTy (s1 :: TyRtl) (w1 :: BW) (s2 :: TyRtl) (w2 :: BW) where
  AddTy 'Unsigned w1 'Unsigned w2 = '( 'Unsigned, Lit 1 + Max w1 w2, 'Male)
  AddTy 'Unsigned w1 'Signed   w2 = '( 'Signed,   Lit 2 + Max w1 (w2 - Lit 1), 'Male)
  AddTy 'Signed   w1 'Unsigned w2 = '( 'Signed,   Lit 2 + Max (w1 - Lit 1) w2, 'Male)
  AddTy 'Signed   w1 'Signed   w2 = '( 'Signed,   Lit 1 + Max w1 w2, 'Male)
-- AddTy _         _  _         _  = Void

