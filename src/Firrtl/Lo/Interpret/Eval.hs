{-# language TypeInType #-}
module Firrtl.Lo.Interpret.Eval where

import Prelude hiding (lookup)

import           Firrtl.Lo.Interpret.Value
import qualified Firrtl.Lo.Syntax.Safe.Expr as SE
import           Firrtl.Lo.TypeCheck.Monad
import           Firrtl.Lo.TypeCheck.Ty

eval :: forall (t :: Ty). Context Int -> SE.Expr t -> Maybe Value
eval env = SE.unK . SE.hcata (evalAlg env)

evalAlg
  :: forall (t :: Ty)
  .  Context Int
  -> SE.ExprF (SE.K (Maybe Value)) t
  -> SE.K (Maybe Value) t
evalAlg _ (SE.UInt _ n) = SE.K (Just (fromIntegral n))

evalAlg _ (SE.SInt _ i) = SE.K (Just i)

-- | if name is not found then we have dangling connection
-- and its value is not valid - Nothing
-- although wires and nodes should have been always connected
-- and regs should contain old value in case of not connected
-- it seems that our typechecker won't find those cases yet
-- TODO: add connection checking step?
evalAlg env (SE.Ref _ ident) = SE.K (lookup ident env)

evalAlg _ (SE.Valid _ cond sig) = SE.K $
  SE.unK cond >>= \c ->
    if c == 1
       then SE.unK sig  >>= \s -> Just s
       else Nothing

evalAlg _ (SE.Mux _ cond ma mb) = SE.K $
  SE.unK cond >>= \c ->
    if c == 0
       then SE.unK mb >>= \b -> Just b
       else SE.unK ma >>= \a -> Just a

evalAlg _ (SE.Add _ ma mb) = SE.K $
  (+) <$> SE.unK ma <*> SE.unK mb
