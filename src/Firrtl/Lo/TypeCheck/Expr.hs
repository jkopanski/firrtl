{-# language
        ScopedTypeVariables
      , TypeInType #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Firrtl.Lo.TypeCheck.Expr where

import Prelude hiding (lookup)
import Data.Maybe              (fromMaybe)
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Prelude hiding (Error)
import Data.Width
import Numeric.Natural         (Natural)

import           Firrtl.Lo.Context
import           Firrtl.Lo.Syntax.Expr
import qualified Firrtl.Lo.Syntax.Safe as Safe
import           Firrtl.Lo.TypeCheck.Monad
import           Firrtl.Lo.TypeCheck.Ty

instance Typed Expr where
  type TypeSafe Expr = Safe.SomeExpr

  typeSafe :: Expr -> Check Safe.SomeExpr
  typeSafe e = do
    ctx <- get
    either throwError pure $ cataM (alg ctx) e

-- | this is awesome as actually only statements can extend Context
alg :: Context (TyRtl, Width, Gender) -> ExprF Safe.SomeExpr -> Either Error Safe.SomeExpr
alg ctx (RefF ident) =
  let mty = lookup ident ctx
   in case mty of
        Just ty -> Right $ case toSing ty of
          SomeSing s -> Safe.fromExpr (Safe.Ref s ident) -- Right expr
        Nothing -> Left $ NotInScope ident Nothing

alg _ (LitF l) = case l of
  UInt mwidth value ->
    let minWidth = minUIntBitWidth value
        width = fromMaybe minWidth mwidth
     in if minWidth > width
           then Left $ NotEnoughWidth l minWidth 
           else Right $ case toSing width of
                        SomeSing sn ->
                          let s = STuple3 SUnsigned sn SMale
                           in Safe.fromExpr (Safe.UInt s value)

  SInt mwidth value ->
    let minWidth = minSIntBitWidth value
        width = fromMaybe minWidth mwidth
     in if minWidth > width
           then Left $ NotEnoughWidth l minWidth 
           else Right $ case toSing width of
                        SomeSing sn ->
                          let s = STuple3 SSigned sn SMale
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

minUIntBitWidth :: Natural -> Width
minUIntBitWidth = (+) 1
            . fromIntegral
            . (floor :: Double -> Int)
            . logBase 2
            . fromIntegral

minSIntBitWidth :: Int -> Width
minSIntBitWidth x | x > 0 = 1 + minUIntBitWidth (fromIntegral x)
                  | otherwise = ( (+) 1
                                . fromIntegral
                                . (ceiling :: Double -> Int)
                                . logBase 2
                                . abs
                                . fromIntegral
                                ) x
