module Firrtl.Lo.Syntax
  ( Syntax (Top)
  , module S
  ) where

import Firrtl.Lo.Syntax.Circuit as S
import Firrtl.Lo.Syntax.Common  as S
import Firrtl.Lo.Syntax.Expr    as S
import Firrtl.Lo.Syntax.Stmt    as S

newtype Syntax = Top Circuit
