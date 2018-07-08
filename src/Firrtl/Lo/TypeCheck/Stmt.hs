{-# language
        DataKinds
      , TypeInType #-}
module Firrtl.Lo.TypeCheck.Stmt where

import Control.Monad.Except (throwError)
import Control.Monad.State  (modify)
import Data.Singletons.Prelude

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
