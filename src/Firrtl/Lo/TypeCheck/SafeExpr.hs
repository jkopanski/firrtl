{-# language
        DataKinds
      , ScopedTypeVariables
      , TypeApplications
      , TypeInType #-}
module Firrtl.Lo.TypeCheck.SafeExpr where

import Control.Monad.Except (throwError)
import Data.Functor.Foldable
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Prelude hiding (Error)
import Data.Nat
import Numeric.Natural

import qualified Firrtl.Lo.Syntax.Safe as Safe
import           Firrtl.Lo.Syntax.Expr
import           Firrtl.Lo.TypeCheck.Monad
import           Firrtl.Lo.TypeCheck.Ty
-- import           Firrtl.Lo.TypeCheck.Types
import           Firrtl.Lo.TypeCheck.Expr

instance Typed Expr where
  type TypeSafe Expr = Safe.SomeExpr

  typeSafe :: Expr -> Check Safe.SomeExpr
  typeSafe = either throwError pure . cataM alg

alg :: ExprF Safe.SomeExpr -> Either Error Safe.SomeExpr
alg (LitF l) = Right $ case l of
  -- FIXME: calculate or use provided width
  UInt w ->
    let n = fromIntegral w :: Natural
     in case someNatVal n of
          SomeSing sn -> Safe.fromExpr_ (STuple3 SUnsigned sn SMale)
                                        (Safe.TFix $ Safe.UInt n)
  SInt w ->
    let n = fromIntegral w :: Natural
     in case someNatVal n of
          SomeSing sn -> Safe.fromExpr_ (STuple3 SSigned sn SMale)
                                        (Safe.TFix $ Safe.SInt w)

alg (ValidF scond@(Safe.MkSomeExpr sc ec) (Safe.MkSomeExpr ss es)) =
  case sc of
    STuple3 SUnsigned SOne SMale ->
      Right $ Safe.fromExpr_ ss (Safe.TFix $ Safe.Valid ec es)
    _ ->
      Left $ NoTopModule "test"

alg (MuxF scond@(Safe.MkSomeExpr sc ec) (Safe.MkSomeExpr sl el) (Safe.MkSomeExpr sr er)) =
  case sc of
    STuple3 SUnsigned SOne SMale -> case sr %~ sl of
      Proved Refl -> Right $ Safe.fromExpr_ sl (Safe.TFix $ Safe.Mux ec el er)
      Disproved _ -> Left $ NoTopModule "same type"
    _ -> Left $ NoTopModule "conditional signal"
