{-# language
        DataKinds
      , ScopedTypeVariables
      , TypeInType #-}
module Firrtl.Lo.TypeCheck.Expr where

import Data.Functor.Foldable
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Prelude hiding (Error)
import Data.Nat
import Numeric.Natural

import           Firrtl.Lo.Syntax.Expr
import qualified Firrtl.Lo.Syntax.Safe as Safe
import           Firrtl.Lo.TypeCheck.Monad
import           Firrtl.Lo.TypeCheck.Ty
-- import           Firrtl.Lo.TypeCheck.Expr

instance Typed Expr where
  type TypeSafe Expr = Safe.SomeExpr

  typeSafe :: Expr -> Check Safe.SomeExpr
  typeSafe e = do
    ctx <- get
    either throwError pure $ cataM (alg ctx) e

-- | this is awesome as actually only statements can extend Context
alg :: Context -> ExprF Safe.SomeExpr -> Either Error Safe.SomeExpr
alg ctx (RefF ident) =
  let mexpr = lookupNode ident ctx
   in case mexpr of
        Just expr -> Right expr
        Nothing   -> Left $ NotInScope ident Nothing

alg _ (LitF l) = Right $ case l of
  UInt width value ->
     case someNatVal width of
       SomeSing sn -> let s = STuple3 SUnsigned sn SMale
                       in Safe.fromExpr (Safe.UInt s value)

  SInt width value ->
     case someNatVal width of
       SomeSing sn -> let s = STuple3 SSigned sn SMale
                       in Safe.fromExpr (Safe.SInt s value)

alg _ (ValidF (Safe.MkSomeExpr sc ec) (Safe.MkSomeExpr ss es)) =
  case sc of
    STuple3 SUnsigned SOne SMale ->
      Right $ Safe.fromExpr (Safe.Valid ss ec es)
    _ ->
      Left $ NoTopModule "test"

alg _ (MuxF (Safe.MkSomeExpr sc ec) (Safe.MkSomeExpr sl el) (Safe.MkSomeExpr sr er)) =
  case sc of
    STuple3 SUnsigned SOne SMale -> case sr %~ sl of
      Proved Refl -> Right $ Safe.fromExpr (Safe.Mux sl ec el er)
      Disproved _ -> Left $ NoTopModule "same type"
    _ -> Left $ NoTopModule "conditional signal"
