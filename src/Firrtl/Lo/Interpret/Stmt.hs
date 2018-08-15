{-# language TypeInType #-}
module Firrtl.Lo.Interpret.Stmt where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Width

import qualified Firrtl.Lo.Syntax.Safe.Expr as SE
import Firrtl.Lo.Syntax.Common    (Id)
import Firrtl.Lo.Syntax.Safe.Stmt
import Firrtl.Lo.TypeCheck.Monad
import Firrtl.Lo.TypeCheck.Ty
import Firrtl.Lo.Interpret.Eval
import Firrtl.Lo.Interpret.Monad
import Firrtl.Lo.Interpret.Value

connectionTarget
  :: forall (t :: Ty) (s :: TyRtl) (w :: BW) (g :: Gender)
  .  ( IsValidLHS g ~ 'True
     , t ~ '(s, w, g)
     )
  => Context Int
  -> SE.Expr t
  -> Maybe Id
connectionTarget _ (SE.TFix (SE.Ref _ ident)) = Just ident
connectionTarget env (SE.TFix (SE.Mux _ cond a b)) =
  eval env cond >>= \c ->
    connectionTarget env $ if c == 1 then a
                                     else b

connectionTarget env (SE.TFix (SE.Valid _ cond sig)) =
  eval env cond >>= \c ->
    if c == 1
       then connectionTarget env sig
       else Nothing

-- FIXME: why ghc complains about non exhoustive patterns without this one?
-- it should be guaranteed that this is Male expression
connectionTarget env (SE.TFix (SE.Add _ _ _)) = undefined

interpret :: Stmt -> Interpret ()
interpret (Block stmts) = sequence_ (interpret <$> stmts)

interpret (Connect lhs rhs) = do
  ctx <- ask
  st  <- get
  let mtarget = connectionTarget (ctx <> st) lhs
      mval    = eval ctx rhs
      action  = case mtarget of
        Just ident -> case mval of
          Just val -> insert ident val
          Nothing  -> delete ident
        Nothing -> id
  modify action

interpret Empty = pure ()

interpret (Node ident expr) = do
  ctx <- ask
  let mval = eval ctx expr
      action = case mval of
                 Just val -> insert ident val
                 Nothing  -> delete ident
  modify action
