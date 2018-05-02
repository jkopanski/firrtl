{-# language MultiWayIf #-}
module Firrtl.Lo.Parser.Circuit where

import Prelude hiding (mod)
import Control.Applicative      ((<|>))
import Data.Functor             (($>))
import Data.List.NonEmpty       (fromList)
import Data.Maybe               (fromMaybe)
import Firrtl.Lo.Parser.Monad
import Firrtl.Lo.Parser.Stmt
import Firrtl.Lo.Syntax
import Firrtl.Lo.TypeCheck.Types
import Text.Megaparsec          (MonadParsec, Pos, Token)
import Text.Parser.Combinators
import Text.Parser.Token

import qualified Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer

circuit :: Parser Circuit
circuit = Text.Megaparsec.Char.Lexer.nonIndented space $
  Text.Megaparsec.Char.Lexer.indentBlock space circ
  where
    circ = do
      reserved "circuit"
      name <- (identifier <?> "circuit name") <* symbolic ':'
      pure $ Text.Megaparsec.Char.Lexer.IndentSome
             Nothing
             (pure . Circuit name . fromList)
             mod

data ModuleType = Standard | External

mod :: Parser Module
mod = do
  space
  ref <- Text.Megaparsec.Char.Lexer.indentLevel
  moduleType <- reserved "module" $> Standard
            <|> reserved "extmodule" $> External
  name <- (identifier <?> "module name") <* symbolic ':'

  -- grab indentation level
  lvl <- Text.Megaparsec.Char.Lexer.indentGuard space GT ref
  -- get ports
  miface <- optional $ indentedItems ref lvl space port
  -- top level modules won't have any I/O
  let iface = fromMaybe [] miface

  case moduleType of
    External -> pure (ExtModule name iface)
    Standard ->
      -- grab module body using same indentation level as ports
      Module name iface . Block <$> indentedItems ref lvl space stmt

indentedItems
  :: MonadParsec e s m
  => Pos               -- ^ Reference indentation level
  -> Pos               -- ^ Level of the first indented item ('lookAhead'ed)
  -> m ()              -- ^ How to consume indentation (white space)
  -> m b               -- ^ How to parse indented tokens
  -> m [b]
indentedItems ref lvl sc p = go
  where
    go = do
      sc
      pos <- Text.Megaparsec.Char.Lexer.indentLevel
      mp <- optional p
      case mp of
        Nothing -> pure []
        Just item ->
          if | pos <= ref -> pure []
             | pos == lvl -> (item:) <$> go
             | otherwise  -> Text.Megaparsec.Char.Lexer.incorrectIndent EQ lvl pos

circModule :: Parser Module
circModule = try extmodule <|> rtlmodule

extmodule :: Parser Module
extmodule = Text.Megaparsec.Char.Lexer.indentBlock space ext
  where
    ext = do
      reserved "extmodule"
      name <- identifier <?> "module name"
      _ <- symbolic ':'
      pure $ Text.Megaparsec.Char.Lexer.IndentSome
             Nothing
             (pure . ExtModule name)
             port

rtlmodule :: Parser Module
rtlmodule = do
  space
  ref <- Text.Megaparsec.Char.Lexer.indentLevel
  reserved "module"
  name <- identifier <?> "module name"
  _ <- symbolic ':'

  lvl <- Text.Megaparsec.Char.eol
         *> Text.Megaparsec.Char.Lexer.indentGuard space GT ref
  ports <- many $ try (indentedItem lvl space port)

  s <- some $ try (indentedItem lvl space stmt) <?> "module body"

  _ <- Text.Megaparsec.Char.eol

  pure $ Module name ports (Block s)

indentedItem :: (MonadParsec e s m, Token s ~ Char)
  => Pos  -- ^ Required indentation level
  -> m () -- ^ How to consume indentation (white space)
  -> m b  -- ^ How to parse indented token
  -> m b
indentedItem ref sc p = Text.Megaparsec.Char.Lexer.indentGuard sc EQ ref *> p

port :: (Monad m, TokenParsing m) => m Port
port = do
  gender <- direction
  name <- identifier
  _ <- symbolic ':'
  ty <- typeDecl
  pure $ Port name (gender ty)

direction :: (Monad m, TokenParsing m) => m (Type -> ConnType)
direction = reserved "input"  $> male
        <|> reserved "output" $> female
