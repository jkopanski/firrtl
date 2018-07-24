module Firrtl.Lo.Parser.Stmt where

import Prelude hiding (print)
import Control.Applicative      ((<|>))
import Data.Functor             (($>))
import Firrtl.Lo.Parser.Expr
import Firrtl.Lo.Parser.Monad
import Firrtl.Lo.Syntax.Expr
import Firrtl.Lo.Syntax.Stmt
import Firrtl.Lo.TypeCheck.Ty
import Numeric.Natural          (Natural)
import Text.Parser.Combinators
import Text.Parser.Token

stmt :: (Monad m, TokenParsing m) => m Stmt
stmt = empty
   <|> exprFirstStmt
   <|> node
   <|> print
   <|> stop
   <|> wire

block, empty, node, print, stop, wire
  :: (Monad m, TokenParsing m) => m Stmt
block = Block <$> some stmt
empty = reserved "skip" $> Empty <?> "empty statement"
node = reserved "node" *> (Node <$> (identifier <* symbolic '=') <*> expr)

print = Print
    <$> (reserved "printf" *> symbolic '('
     *> expr <?> "clock signal")
    <*> (comma *> expr <?> "print condition signal")
    <*> (comma *> stringLiteral <?> "print format srting")
    <*> option [] (comma *> commaSep1 expr)
    <* symbolic ')'

stop = Stop
   <$> (reserved "stop" *> symbolic '('
    *> expr <?> "clock signal")
   <*> (comma *> expr <?> "halt condition signal")
   <*> (comma *> (fromIntegral <$> integer) <* symbolic ')' <?> "exit code")

wire = do
  reserved "wire"
  ident <- identifier
  symbolic ':'
  (ty, width) <- sizedType
  pure $ Wire ident (ty, width, Bi)

-- ^ statements which are starting with expression
-- | combined to single function to avoid backtracking
-- | `connect`, `invalide` and `partial connect`
exprFirstStmt :: (Monad m, TokenParsing m) => m Stmt
exprFirstStmt = do
  ex <- expr
  choice [ connect ex
         , invalid ex
         -- , partial
         ]
  where
    connect :: (Monad m, TokenParsing m) => Expr -> m Stmt
    connect e = reserved "<=" *> (Connect e <$> expr)
    invalid :: (Monad m, TokenParsing m) => Expr -> m Stmt
    invalid e = reserved "is" *> reserved "invalid" $> Invalid e

typeDecl :: (Monad m, TokenParsing m) => m TyRtl
typeDecl = clock
       <|> signed
       <|> unsigned

sizedType :: (Monad m, TokenParsing m) => m (TyRtl, Natural)
sizedType = do
  ty <- typeDecl
  case ty of
    Clock -> pure $ (ty, 1)
    _ -> do
      width <- size
      pure $ (ty, width)

clock, signed, unsigned :: (Monad m, TokenParsing m) => m TyRtl
clock = reserved "Clock" $> Clock
signed = reserved "SInt" $> Signed
unsigned = reserved "UInt" $> Unsigned
