{-|
Type checker for FIRRTL expressions.
This actually "infers" type of an expression.
Type inference is actually a bit of an exaggeration
since we just follow the an @Expr@ till we
get to an @Ref@ and get type from environment.
|-}
module Firrtl.Lo.TypeCheck.Expr where

import           Prelude                    hiding (lookup)
import           Control.Monad.Except       (throwError)
import           Control.Monad.Reader       (asks)

import           Firrtl.Lo.Syntax.Expr
import           Firrtl.Lo.TypeCheck.Monad
import           Firrtl.Lo.TypeCheck.Types

typeof :: Expr -> Check ConnType
typeof (Ref ident) = asks (lookup ident) >>= \case
  Nothing -> throwError $ NotInScope ident Nothing
  Just t  -> pure t

typeof (Parameter (Imm n)) = pure $ male $ Natural n

typeof (Lit l) = case l of
  UInt i -> pure $ male $ Unsigned (minBitWidth i)
  SInt i -> pure $ male $ Signed (minSignedBitWidth i)

typeof (Valid cond e) = do
  t <- typeof cond
  case connType t of
    (Unsigned 1) -> typeof e
    _            -> throwError $ Mismatch (Unsigned 1) (connType t)

typeof (Mux sel l r) = do
  t <- typeof sel
  case connType t of
    (Unsigned 1) -> do
      tl <- typeof l
      tr <- typeof r
      if tl == tr
        then pure tl
        else throwError $ Mismatch (connType tl) (connType tr)
    _            -> throwError $ Mismatch (Unsigned 1) (connType t)

typeof (Unary op a) = do
  t <- typeof a
  case typeofUnary op (connType t) of
    Right ret -> pure (ConnType (connGender t) ret)
    Left  err -> throwError err

typeof (Binary op l r) = do
  tl <- typeof l
  tr <- typeof r
  case typeofBinary op (connType tl) (connType tr) of
    Right ret -> pure (ConnType (connGender tl) ret)
    Left  err -> throwError err

typeof (Ternary op p q r) = do
  tp <- typeof p
  tq <- typeof q
  tr <- typeof r
  case typeofTernary op (connType tp) (connType tq) (connType tr) of
    Right ret -> pure (ConnType (connGender tp) ret)
    Left  err -> throwError err

typeofUnary :: UnaryOp -> Type -> Either Error Type
typeofUnary AndR (Unsigned _) = Right $ Unsigned 1
typeofUnary AndR (Signed   _) = Right $ Unsigned 1
typeofUnary AndR t = Left $ ExpectedGroundInt t

typeofUnary AsClock (Unsigned _) = Right Clock
typeofUnary AsClock (Signed   _) = Right Clock
typeofUnary AsClock Clock        = Right Clock
typeofUnary AsClock t = Left $ ExpectedGround t

typeofUnary AsSigned (Unsigned w) = Right $ Signed w
typeofUnary AsSigned (Signed   w) = Right $ Signed w
typeofUnary AsSigned Clock        = Right $ Signed 1
typeofUnary AsSigned t = Left $ ExpectedGround t

typeofUnary AsUnsigned (Unsigned w) = Right $ Unsigned w
typeofUnary AsUnsigned (Signed   w) = Right $ Unsigned w
typeofUnary AsUnsigned Clock        = Right $ Unsigned 1
typeofUnary AsUnsigned t = Left $ ExpectedGround t

typeofUnary Cvt (Unsigned w) = Right $ Signed (w + 1)
typeofUnary Cvt (Signed   w) = Right $ Signed w
typeofUnary Cvt t = Left $ ExpectedGround t

typeofUnary Neg (Unsigned w) = Right $ Signed (w + 1)
typeofUnary Neg (Signed   w) = Right $ Signed w
typeofUnary Neg t = Left $ ExpectedGroundInt t

typeofUnary Not (Unsigned w) = Right $ Unsigned w
typeofUnary Not (Signed   w) = Right $ Unsigned (w + 1)
typeofUnary Not t = Left $ ExpectedGroundInt t

typeofUnary OrR (Unsigned _) = Right $ Unsigned 1
typeofUnary OrR (Signed   _) = Right $ Unsigned 1
typeofUnary OrR t = Left $ ExpectedGroundInt t

typeofUnary XorR (Unsigned _) = Right $ Unsigned 1
typeofUnary XorR (Signed   _) = Right $ Unsigned 1
typeofUnary XorR t = Left $ ExpectedGroundInt t

typeofBinary :: BinaryOp -> Type -> Type -> Either Error Type
typeofBinary Add (Unsigned w1) (Unsigned w2) = Right $ Unsigned (1 + max w1 w2)
typeofBinary Add (Unsigned w1) (Signed   w2) = Right $ Signed   (2 + max w1 (w2 - 1))
typeofBinary Add (Signed   w1) (Unsigned w2) = Right $ Unsigned (2 + max w2 (w1 - 1))
typeofBinary Add (Signed   w1) (Signed   w2) = Right $ Signed   (1 + max w1 w2)
typeofBinary Add l r = Left $ expectedBinary l r

typeofBinary And (Unsigned w1) (Unsigned w2) = Right $ Unsigned (max w1 w2)
typeofBinary And (Unsigned w1) (Signed   w2) = Right $ Unsigned (max w1 w2)
typeofBinary And (Signed   w1) (Unsigned w2) = Right $ Unsigned (max w1 w2)
typeofBinary And (Signed   w1) (Signed   w2) = Right $ Unsigned (max w1 w2)
typeofBinary And l r = Left $ expectedBinary l r

typeofBinary Cat (Unsigned w1) (Unsigned w2) = Right $ Unsigned (w1 + w2)
typeofBinary Cat (Unsigned w1) (Signed   w2) = Right $ Unsigned (w1 + w2)
typeofBinary Cat (Signed   w1) (Unsigned w2) = Right $ Unsigned (w1 + w2)
typeofBinary Cat (Signed   w1) (Signed   w2) = Right $ Unsigned (w1 + w2)
typeofBinary Cat l r = Left $ expectedBinary l r

typeofBinary Div (Unsigned w1) (Unsigned _) = Right $ Unsigned w1
typeofBinary Div (Unsigned w1) (Signed   _) = Right $ Signed   (w1 + 1)
typeofBinary Div (Signed   w1) (Unsigned _) = Right $ Signed   w1
typeofBinary Div (Signed   w1) (Signed   _) = Right $ Signed   (w1 + 1)
typeofBinary Div l r = Left $ expectedBinary l r

typeofBinary DShl (Unsigned w1) (Unsigned w2) = Right $ Unsigned (w1 + 2 ^ w2 - 1)
typeofBinary DShl (Signed   w1) (Unsigned w2) = Right $ Signed   (w1 + 2 ^ w2 - 1)
typeofBinary DShl l r = Left $ expectedBinary l r

typeofBinary DShr (Unsigned w1) (Unsigned w2) = Right $ Unsigned (w1 + 2 ^ w2 - 1)
typeofBinary DShr (Signed   w1) (Unsigned w2) = Right $ Signed   (w1 + 2 ^ w2 - 1)
typeofBinary DShr l r = Left $ expectedBinary l r

typeofBinary Eq  (Unsigned _) (Unsigned _) = Right $ Unsigned 1
typeofBinary Eq  (Unsigned _) (Signed   _) = Right $ Unsigned 1
typeofBinary Eq  (Signed   _) (Unsigned _) = Right $ Unsigned 1
typeofBinary Eq  (Signed   _) (Signed   _) = Right $ Unsigned 1
typeofBinary Eq  l r = Left $ expectedBinary l r

typeofBinary Geq (Unsigned _) (Unsigned _) = Right $ Unsigned 1
typeofBinary Geq (Unsigned _) (Signed   _) = Right $ Unsigned 1
typeofBinary Geq (Signed   _) (Unsigned _) = Right $ Unsigned 1
typeofBinary Geq (Signed   _) (Signed   _) = Right $ Unsigned 1
typeofBinary Geq l r = Left $ expectedBinary l r

typeofBinary Gt  (Unsigned _) (Unsigned _) = Right $ Unsigned 1
typeofBinary Gt  (Unsigned _) (Signed   _) = Right $ Unsigned 1
typeofBinary Gt  (Signed   _) (Unsigned _) = Right $ Unsigned 1
typeofBinary Gt  (Signed   _) (Signed   _) = Right $ Unsigned 1
typeofBinary Gt  l r = Left $ expectedBinary l r

typeofBinary Leq (Unsigned _) (Unsigned _) = Right $ Unsigned 1
typeofBinary Leq (Unsigned _) (Signed   _) = Right $ Unsigned 1
typeofBinary Leq (Signed   _) (Unsigned _) = Right $ Unsigned 1
typeofBinary Leq (Signed   _) (Signed   _) = Right $ Unsigned 1
typeofBinary Leq l r = Left $ expectedBinary l r

typeofBinary Lt  (Unsigned _) (Unsigned _) = Right $ Unsigned 1
typeofBinary Lt  (Unsigned _) (Signed   _) = Right $ Unsigned 1
typeofBinary Lt  (Signed   _) (Unsigned _) = Right $ Unsigned 1
typeofBinary Lt  (Signed   _) (Signed   _) = Right $ Unsigned 1
typeofBinary Lt  l r = Left $ expectedBinary l r

typeofBinary Mod (Unsigned w1) (Unsigned w2) = Right $ Unsigned (min w1 w2)
typeofBinary Mod (Unsigned w1) (Signed   w2) = Right $ Unsigned (min w1 w2)
typeofBinary Mod (Signed   w1) (Unsigned w2) = Right $ Signed   (min w1 (w2 + 1))
typeofBinary Mod (Signed   w1) (Signed   w2) = Right $ Signed   (min w1 w2)
typeofBinary Mod l r = Left $ expectedBinary l r

typeofBinary Mul (Unsigned w1) (Unsigned w2) = Right $ Unsigned (w1 + w2)
typeofBinary Mul (Unsigned w1) (Signed   w2) = Right $ Signed   (w1 + w2)
typeofBinary Mul (Signed   w1) (Unsigned w2) = Right $ Signed   (w1 + w2)
typeofBinary Mul (Signed   w1) (Signed   w2) = Right $ Signed   (w1 + w2)
typeofBinary Mul l r = Left $ expectedBinary l r

typeofBinary Neq (Unsigned _) (Unsigned _) = Right $ Unsigned 1
typeofBinary Neq (Unsigned _) (Signed   _) = Right $ Unsigned 1
typeofBinary Neq (Signed   _) (Unsigned _) = Right $ Unsigned 1
typeofBinary Neq (Signed   _) (Signed   _) = Right $ Unsigned 1
typeofBinary Neq l r = Left $ expectedBinary l r

typeofBinary Or  (Unsigned w1) (Unsigned w2) = Right $ Unsigned (max w1 w2)
typeofBinary Or  (Unsigned w1) (Signed   w2) = Right $ Unsigned (max w1 w2)
typeofBinary Or  (Signed   w1) (Unsigned w2) = Right $ Unsigned (max w1 w2)
typeofBinary Or  (Signed   w1) (Signed   w2) = Right $ Unsigned (max w1 w2)
typeofBinary Or  l r = Left $ expectedBinary l r

typeofBinary Sub (Unsigned w1) (Unsigned w2) = Right $ Signed (1 + max w1 w2)
typeofBinary Sub (Unsigned w1) (Signed   w2) = Right $ Signed (max (w1 + 2) (w2 + 1))
typeofBinary Sub (Signed   w1) (Unsigned w2) = Right $ Signed (max (w1 + 1) (w2 + 2))
typeofBinary Sub (Signed   w1) (Signed   w2) = Right $ Signed (1 + max w1 w2)
typeofBinary Sub l r = Left $ expectedBinary l r

typeofBinary Xor (Unsigned w1) (Unsigned w2) = Right $ Unsigned (max w1 w2)
typeofBinary Xor (Unsigned w1) (Signed   w2) = Right $ Unsigned (max w1 w2)
typeofBinary Xor (Signed   w1) (Unsigned w2) = Right $ Unsigned (max w1 w2)
typeofBinary Xor (Signed   w1) (Signed   w2) = Right $ Unsigned (max w1 w2)
typeofBinary Xor l r = Left $ expectedBinary l r

-- | Parametrized operations
typeofBinary Head (Unsigned _) (Natural n) = Right $ Unsigned $ fromIntegral $ toInteger n
typeofBinary Head (Signed   _) (Natural n) = Right $ Unsigned $ fromIntegral $ toInteger n
typeofBinary Head l r = Left $ parametrizedBinary l r

typeofBinary Pad (Unsigned w) (Natural n) = Right $ Unsigned (max w (fromIntegral $ toInteger n))
typeofBinary Pad (Signed   w) (Natural n) = Right $ Signed   (max w (fromIntegral $ toInteger n))
typeofBinary Pad l r = Left $ parametrizedBinary l r

typeofBinary Shl (Unsigned w) (Natural n) = Right $ Unsigned (w + fromIntegral (toInteger n))
typeofBinary Shl (Signed   w) (Natural n) = Right $ Signed   (w + fromIntegral (toInteger n))
typeofBinary Shl l r = Left $ parametrizedBinary l r

typeofBinary Shr (Unsigned w) (Natural nat) =
  let n = fromIntegral $ toInteger nat
  in if n < w
        then Right $ Unsigned (w - n)
        else Left $ ParameterToBig nat (Unsigned w)
typeofBinary Shr (Signed   w) (Natural nat) =
  let n = fromIntegral $ toInteger nat
  in if n < w
        then Right $ Signed (w - n)
        else Left $ ParameterToBig nat (Unsigned w)
typeofBinary Shr l r = Left $ parametrizedBinary l r

typeofBinary Tail (Unsigned w) (Natural nat) =
  let n = fromIntegral $ toInteger nat
  in if n < w
        then Right $ Unsigned (w - n)
        else Left $ ParameterToBig nat (Unsigned w)
typeofBinary Tail (Signed   w) (Natural nat) =
  let n = fromIntegral $ toInteger nat
  in if n < w
        then Right $ Unsigned (w - n)
        else Left $ ParameterToBig nat (Unsigned w)
typeofBinary Tail l r = Left $ parametrizedBinary l r

expectedBinary :: Type -> Type -> Error
expectedBinary (Unsigned _) r = ExpectedGroundInt r
expectedBinary (Signed   _) r = ExpectedGroundInt r
expectedBinary l (Unsigned _) = ExpectedGroundInt l
expectedBinary l _            = ExpectedGroundInt l

parametrizedBinary :: Type -> Type -> Error
parametrizedBinary t n = case n of
  (Natural _) -> ExpectedGroundInt t
  _           -> ExpectedParameter n

typeofTernary :: TernaryOp -> Type -> Type -> Type -> Either Error Type
typeofTernary Bits bits (Natural nl) (Natural nr) =
  let hi = fromIntegral $ toInteger nl
      lo = fromIntegral $ toInteger nr
   in case bits of
    (Unsigned _) -> Right $ Unsigned (hi - lo + 1)
    (Signed   _) -> Right $ Unsigned (hi - lo + 1)
    t            -> Left  $ ExpectedGroundInt t
typeofTernary Bits _ (Natural _) t = Left $ ExpectedParameter t
typeofTernary Bits _ t _ = Left $ ExpectedParameter t
