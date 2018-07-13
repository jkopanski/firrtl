{-# language
        DataKinds
      , TypeInType #-}
module Firrtl.Lo.TypeCheck.Stmt where

import Control.Monad.Except (throwError)
import Control.Monad.State  (modify)
import Data.Singletons.Prelude
import Data.Singletons.Decide

import qualified Firrtl.Lo.Syntax.Safe as Safe
import           Firrtl.Lo.Syntax.Expr
import           Firrtl.Lo.Syntax.Stmt
import           Firrtl.Lo.TypeCheck.Monad
import           Firrtl.Lo.TypeCheck.Ty
import           Firrtl.Lo.TypeCheck.Expr

instance Typed Stmt where
  type TypeSafe Stmt = Safe.Stmt

  typeSafe :: Stmt -> Check Safe.Stmt
  typeSafe (Block stmts) = Safe.Block <$> sequence (typeSafe <$> stmts)

  typeSafe (Connect lexpr rexpr) = do
    lhs <- typeSafe lexpr
    rhs <- typeSafe rexpr
    case (lhs, rhs) of
      (Safe.MkSomeExpr ls le, Safe.MkSomeExpr rs re) -> case (ls, rs) of
        (STuple3 lt ln lg, STuple3 rt rn rg) -> case lt %~ rt of
          Disproved _ -> throwError $ Equivalent (FromSing ls) (FromSing rs)
          Proved Refl -> case (ln %>= rn) %~ STrue of
            Disproved _ -> throwError $ Containable (FromSing ls) (FromSing rs)
            Proved Refl -> case (lg, rg) of
              (SFemale, SBi  ) -> pure (Safe.Connect le re)
              (SFemale, SMale) -> pure (Safe.Connect le re)
              (SBi    , SBi  ) -> pure (Safe.Connect le re)
              (SBi    , SMale) -> pure (Safe.Connect le re)
              (_      , _    ) -> throwError $ Connectable (FromSing ls) (FromSing rs)

  typeSafe Empty = pure Safe.Empty

  typeSafe (Node ident expr) = do
    sexpr <- typeSafe expr
    case sexpr of
      Safe.MkSomeExpr se ee -> case se of
        STuple3 s1 s2 SMale -> do
          withSingI s1 $ withSingI s2 $
            modify (insertNode ident ee)
          pure $ Safe.Node ident ee
        _ -> throwError $ NodeMale (fromSing se)
