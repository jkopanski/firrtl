{-|
This is based on (riped off) from
<https://github.com/dhall-lang/dhall-haskell/blob/master/src/Dhall/Parser.hs>
which is Copyright Gabriel Gonzalez, licensed under BSD3
|-}
module Firrtl.Lo.Parser.Monad where

import Control.Applicative  (Alternative (..), liftA2)
import Control.Monad        (MonadPlus)
import Data.Functor         (($>))
import Data.HashSet         (HashSet)
import Data.Monoid          (Monoid (..))
import Data.Semigroup       (Semigroup (..))
import Data.String          (IsString (..))
import Data.Text.Lazy       (Text)
import Data.Void            (Void)
import Formatting.Buildable (Buildable (..))
import Text.Parser.Token    (TokenParsing(..))

import qualified Data.Char
import qualified Data.HashSet
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer
import qualified Text.Parser.Combinators
import qualified Text.Parser.Char
import qualified Text.Parser.Token
import qualified Text.Parser.Token.Highlight
import qualified Text.Parser.Token.Style

import Firrtl.Lo.TypeCheck.Types

data Loc = Loc Text.Megaparsec.SourcePos Text.Megaparsec.SourcePos Text
  deriving (Eq, Show)

instance Buildable Loc where
  build (Loc begin _ text)
    = build text <> "\n"
   <> "\n"
   <> build (show begin)
   <> "\n"

newtype Parser a = Parser { unParser :: Text.Megaparsec.Parsec Void Text a }
  deriving ( Functor
           , Applicative
           , Monad
           , Alternative
           , MonadPlus
           , Text.Megaparsec.MonadParsec Void Text
           )

-- nonNewlineSpace :: Parser (
nonNewlineSpace :: (Text.Megaparsec.MonadParsec e s m, Text.Megaparsec.Token s ~ Char) => m ()
nonNewlineSpace = Text.Megaparsec.skipSome
  (Text.Megaparsec.Char.satisfy (\c -> Data.Char.isSpace c && (c /= '\n')))

space :: Parser ()
space = Text.Megaparsec.Char.Lexer.space
  Text.Megaparsec.Char.space1
  lineComment
  Control.Applicative.empty

lineComment :: Parser ()
lineComment = Text.Megaparsec.Char.Lexer.skipLineComment ";"

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (Parser a) where
  mempty = pure mempty
  mappend = (<>)

instance IsString a => IsString (Parser a) where
  fromString x = fromString x <$ Text.Megaparsec.Char.string (fromString x)

instance Text.Parser.Combinators.Parsing Parser where
  try = Text.Megaparsec.try
  (<?>) = (Text.Megaparsec.<?>)
  skipMany = Text.Megaparsec.skipMany
  skipSome = Text.Megaparsec.skipSome
  unexpected = fail
  eof = Parser Text.Megaparsec.eof
  notFollowedBy = Text.Megaparsec.notFollowedBy

instance Text.Parser.Char.CharParsing Parser where
  satisfy = Parser . Text.Megaparsec.Char.satisfy
  char = Text.Megaparsec.Char.char
  notChar = Text.Megaparsec.Char.char
  anyChar = Text.Megaparsec.Char.anyChar
  string = fmap Data.Text.Lazy.unpack . Text.Megaparsec.Char.string . fromString
  text = fmap Data.Text.Lazy.toStrict . Text.Megaparsec.Char.string . Data.Text.Lazy.fromStrict

instance TokenParsing Parser where
  -- TODO: remove new line from space parsing
  someSpace =
    Text.Parser.Token.Style.buildSomeSpaceParser
      (Parser (Text.Megaparsec.skipSome nonNewlineSpace))
      Text.Parser.Token.Style.emptyCommentStyle { Text.Parser.Token.Style._commentLine = ";" }

  highlight _ = id

  semi = token (Text.Megaparsec.Char.char ';' Text.Megaparsec.<?> ";")

  -- nesting = undefined
  -- token = undefined

-- data ParserState = ParserState
--   { indentStack :: [Int]
--   , parenStack  :: [Maybe Int]
--   -- , memoryState :: Mem -- ^ state to parse memory block
--   }

-- type RTLParser = StateT ParserState RTLInnerParser

-- newtype RTLInnerParser a = RTLInnerParser { runInnerParser :: Parser a }
--   deriving (Applicative, Alternative, Functor, Monad, MonadPlus, Monoid,
--             CharParsing, DeltaParsing, LookAheadParsing, TokenParsing)

-- deriving instance Parsing RTLInnerParser

-- testParser :: (MonadIO m, Show a) => RTLParser a -> String -> m ()
-- testParser p = parseTest (runInnerParser (evalStateT p (ParserState [] [] {- emptyMem -})))
parseTest :: Parser a -> Text -> Maybe a
parseTest p = Text.Megaparsec.parseMaybe (unParser p)

idents :: TokenParsing m => Text.Parser.Token.IdentifierStyle m
idents = Text.Parser.Token.IdentifierStyle
  { Text.Parser.Token._styleName = "identifier"
  , Text.Parser.Token._styleStart = Text.Parser.Char.letter <|> Text.Parser.Char.char '_'
  , Text.Parser.Token._styleLetter = Text.Parser.Char.alphaNum <|> Text.Parser.Char.char '_'
  , Text.Parser.Token._styleReserved = Data.HashSet.map Data.Text.Lazy.unpack (keywords <> primOps)
  , Text.Parser.Token._styleHighlight = Text.Parser.Token.Highlight.Identifier
  , Text.Parser.Token._styleReservedHighlight = Text.Parser.Token.Highlight.ReservedIdentifier
  }

keywords :: HashSet Text
keywords = Data.HashSet.fromList
  [ "Analog", "attach"
  , "circuit", "Clock"
  , "data-type", "depth"
  , "else", "extmodule"
  , "Fixed", "flip"
  , "input", "inst", "invalid", "is"
  , "mem", "module", "mux"
  , "new", "node"
  , "old", "output"
  , "printf"
  , "reader", "readwriter", "read-latency", "read-under-write", "reg"
  , "SInt", "skip", "stop"
  , "undefined", "UInt"
  , "when", "wire", "writer", "write-latency"
  , "validif"
  , "<=", "=", "=>"
  ]

primOps :: HashSet Text
primOps = Data.HashSet.fromList
  [ "add"
  , "sub"
  , "mul"
  , "div"
  , "mod"
  , "lt"
  , "leq"
  , "gt"
  , "geq"
  , "eq"
  , "neq"
  , "pad"
  , "asUInt"
  , "asSInt"
  , "asClock"
  , "shl"
  , "shr"
  , "dshl"
  , "dshr"
  , "cvt"
  , "neg"
  , "not"
  , "and"
  , "or"
  , "xor"
  , "andr"
  , "orr"
  , "xorr"
  , "cat"
  , "bits"
  , "head"
  , "tail"
  ]

identifier :: (Monad m, TokenParsing m) => m Text
identifier = Text.Parser.Token.ident idents

reserved :: (Monad m, TokenParsing m) => Data.Text.Text -> m ()
reserved = Text.Parser.Token.reserveText idents

ground :: (Monad m, TokenParsing m) => m Type
ground = reserved "UInt" *>
  (Unsigned <$> fmap fromInteger (Text.Parser.Token.angles Text.Parser.Token.natural))
     <|> reserved "SInt" *>
  (Signed <$> fmap fromInteger (Text.Parser.Token.angles Text.Parser.Token.natural))
     <|> (reserved "Clock" $> Clock)
     <|> Natural <$> fmap fromInteger Text.Parser.Token.natural
