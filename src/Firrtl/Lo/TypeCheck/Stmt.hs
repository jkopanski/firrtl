{-|
Type checker for FIRRTL statements.
Checks if Statemets are legal.
|-}
module Firrtl.Lo.TypeCheck.Stmt where

import Prelude hiding (lookup)

import Control.Monad        (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Firrtl.Lo.Syntax.Stmt
import Firrtl.Lo.TypeCheck.Expr
import Firrtl.Lo.TypeCheck.Monad
import Firrtl.Lo.TypeCheck.Types

check :: Stmt -> Check ()
check (Block stmts) = mapM_ check stmts

check (Connect target source) = do
  tytarget <- typeof target
  tysource <- typeof source
  unless (tytarget `connectable` tysource) (throwError $ Connectable tytarget tysource)
  unless (tytarget `containable` tysource) (throwError $ Containable tytarget tysource)
  unless (tytarget `equivalent` tysource) (throwError $ Equivalent tytarget tysource)
  pure ()

check (Node ident expr) = do
  ty <- typeof expr
  asks (lookup ident)
    >>= \case
      Nothing -> throwError $ NotInScope ident (Just $ connType ty)
      Just tyid -> unless (tyid == ty) (throwError $ Mismatch (connType ty) (connType tyid))

check (Print clk cond _ _) = do
  tyclk <- typeof clk
  tycond <- typeof cond
  let contyclk = connType tyclk
      contycond = connType tycond
  unless (contyclk == Clock) (throwError $ Mismatch Clock contyclk)
  unless (contycond == Unsigned 1) (throwError $ Mismatch (Unsigned 1) contycond)

check (Stop clk halt _) = do
  tyclk <- typeof clk
  tyhalt <- typeof halt
  let contyclk = connType tyclk
      contyhalt = connType tyhalt
  unless (contyclk == Clock) (throwError $ Mismatch Clock contyclk)
  unless (contyhalt == Unsigned 1) (throwError $ Mismatch (Unsigned 1) contyhalt)

check Empty = pure ()
check (Invalid _) = pure ()
check (Wire _ _) = pure ()
