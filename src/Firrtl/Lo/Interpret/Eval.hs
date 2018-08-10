{-# language TypeInType #-}
module Firrtl.Lo.Interpret.Eval where

import Prelude hiding (lookup)
-- import Data.Kind  (type (*))
import Data.Maybe (fromJust)
import Data.Singletons

import           Firrtl.Lo.Interpret.Value
import qualified Firrtl.Lo.Syntax.Safe.Expr as SE
import           Firrtl.Lo.TypeCheck.Monad
import           Firrtl.Lo.TypeCheck.Ty

eval :: forall (t :: Ty). Context Int -> SE.Expr t -> Value t
eval env = SE.hcata (evalAlg env)

evalAlg
  :: forall (t :: Ty)
  .  Context Int
  -> SE.ExprF Value t
  -> Value t
evalAlg _ (SE.UInt s u) = Valid s (fromIntegral u)  
evalAlg _ (SE.SInt s i) = Valid s i
-- TypeChecking should make us suer that we have Just here
evalAlg env (SE.Ref s ident) = Valid s (fromJust $ lookup ident env)

evalAlg _ (SE.Valid _ cond sig) =
  if cond == zero then Invalid
                  else sig

evalAlg _ (SE.Mux _ cond a b) =
  if cond == zero then a
                  else b

evalAlg _ (SE.Add s a b) =
  let limit = withSingI s maxBound
   in case (a, b) of
        (Valid _ va, Valid _ vb) ->
          let new = va + vb
              -- TODO: check me, but I think typesystem makes this impossible
              val = if new > limit
                       then new - limit
                       else new
           in Valid s val
        _ -> Invalid
