module Firrtl.Lo.Parser.Expr where

import Control.Applicative
import Data.Functor             (($>))
import Data.List                (foldl')
import Data.Semigroup           ((<>))
import Data.Width
import Firrtl.Lo.Parser.Monad
import Firrtl.Lo.Syntax.Expr
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token hiding (hexadecimal, octal)

import qualified Data.Char

expr :: (Monad m, TokenParsing m) => m Expr
expr = ref
   <|> Parameter <$> immediate
   <|> Lit <$> literal
   <|> valid
   <|> mux
   <|> prim

ref :: (Monad m, TokenParsing m) => m Expr
ref = Ref <$> identifier <?> "Reference"

immediate :: TokenParsing m => m Immediate
immediate = Imm . fromIntegral <$> natural

literal :: (Monad m, TokenParsing m) => m Literal
literal = unsignedLit <|> signedLit

valid :: (Monad m, TokenParsing m) => m Expr
valid = do
  reserved "validif" <?> "Conditional validity"
  args <- parens $ commaSep expr
  if length args /= 2
     then unexpected "Expected 2 arguments to conditional validity expression"
     else let [cond, arg] = args
           in pure $ Valid cond arg

mux :: (Monad m, TokenParsing m) => m Expr
mux = do
  reserved "mux" <?> "Multiplexor keyword"
  args <- parens $ commaSep expr
  if length args /= 3
     then unexpected "Expected 3 arguments to multiplexor"
     else let [cond, a, b] = args
           in pure $ Mux cond a b

prim :: (Monad m, TokenParsing m) => m Expr
prim = do
  op <- primOp
  args <- parens $ commaSep expr
  if length args /= arity op
     then unexpected ("Expected " <> show (arity op) <> " arguments")
     else pure $ case op of
            (U u) -> Unary u $ head args
            (B b) -> let [p, q] = args
                      in Binary b p q
            (T t) -> let [p, q, r] = args
                      in Ternary t p q r

data PrimOp
  = U UnaryOp
  | B BinaryOp
  | T TernaryOp

primOp :: (Monad m, TokenParsing m) => m PrimOp
primOp = U <$> unaryOp
     <|> B <$> binaryOp
     <|> T <$> ternaryOp

arity :: PrimOp -> Int
arity (U _) = 1
arity (B _) = 2
arity (T _) = 3

unaryOp :: (Monad m, TokenParsing m) => m UnaryOp
unaryOp = reserved "andr"    $> AndR
      <|> reserved "asClock" $> AsClock
      <|> reserved "asSInt"  $> AsSigned
      <|> reserved "asUInt"  $> AsUnsigned
      <|> reserved "cvt"     $> Cvt
      <|> reserved "neg"     $> Neg
      <|> reserved "not"     $> Not
      <|> reserved "orr"     $> OrR
      <|> reserved "xorr"    $> XorR

binaryOp :: (Monad m, TokenParsing m) => m BinaryOp
binaryOp = reserved "add"  $> Add
       <|> reserved "and"  $> And
       <|> reserved "cat"  $> Cat
       <|> reserved "div"  $> Div
       <|> reserved "dshl" $> DShl
       <|> reserved "DShr" $> DShr
       <|> reserved "eq"   $> Eq
       <|> reserved "geq"  $> Geq
       <|> reserved "gt"   $> Gt
       <|> reserved "leq"  $> Leq
       <|> reserved "lt"   $> Lt
       <|> reserved "mod"  $> Mod
       <|> reserved "mul"  $> Mul
       <|> reserved "neq"  $> Neq
       <|> reserved "or"   $> Or
       <|> reserved "sub"  $> Sub
       <|> reserved "xor"  $> Xor
       <|> reserved "head" $> Head
       <|> reserved "pad"  $> Pad
       <|> reserved "shl"  $> Shl
       <|> reserved "shr"  $> Shr
       <|> reserved "tail" $> Tail

ternaryOp :: (Monad m, TokenParsing m) => m TernaryOp
ternaryOp = reserved "bits" $> Bits

-- unfortunately Text.Parser.Token does not export this function
-- https://hackage.haskell.org/package/parsers-0.12.4/docs/src/Text-Parser-Token.html#number
number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  foldl' (\x d -> base*x + toInteger (Data.Char.digitToInt d)) 0 <$> some baseDigit

data Sign = Plus | Minus

sign :: TokenParsing m => m Sign
sign = option Plus (char '-' $> Minus)
{-# INLINE sign #-}

literalBits :: TokenParsing m => Integer -> m Char -> m Char -> m Integer
literalBits base prefix baseDigit = do
  prefix
  s <- sign
  n <- number base (skipMany (char '_') *> baseDigit)
  pure $ case s of
    Plus  -> n
    Minus -> negate n
{-# INLINE literalBits #-}

hexadecimal :: TokenParsing m => m Integer
hexadecimal = literalBits 16 (oneOf "hH") hexDigit <?> "hexadecimal literal"
{-# INLINE hexadecimal #-}

octal :: TokenParsing m => m Integer
octal = literalBits 8 (oneOf "oO") octDigit <?> "octal literal"
{-# INLINE octal #-}

binDigit :: CharParsing m => m Char
binDigit = oneOf "01" <?> "binary digit"
{-# INLINE binDigit #-}

binary :: TokenParsing m => m Integer
binary = literalBits 2 (oneOf "bB") binDigit <?> "binary literal"
{-# INLINE binary #-}

size :: TokenParsing m => m Width
size = angles $ fromInteger <$> natural

value :: TokenParsing m => m Int
value = fromInteger <$> integer

unsignedLit :: (Monad m, TokenParsing m) => m Literal
unsignedLit = do
  reserved "UInt"
  msize <- optional size
  val <- fromInteger <$> parens (decimal <|> (between (char '"') (char '"') (hexadecimal <|> octal <|> binary)))
  pure (UInt msize val)

signedLit :: (Monad m, TokenParsing m) => m Literal
signedLit = do
  reserved "SInt"
  -- TODO: use width hint
  msize <- optional size
  val <- fromInteger <$> parens (integer <|> (between (char '"') (char '"') (hexadecimal <|> octal <|> binary)))
  pure (SInt msize val)
