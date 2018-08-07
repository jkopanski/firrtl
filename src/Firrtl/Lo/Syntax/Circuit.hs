module Firrtl.Lo.Syntax.Circuit where

import Data.List.NonEmpty (NonEmpty)

import Firrtl.Lo.Syntax.Common
import Firrtl.Lo.Syntax.Stmt
import Firrtl.Lo.TypeCheck.Ty

data Circuit
  = Circuit Id                -- circuit name
            (NonEmpty Module) -- modules
  deriving (Eq, Show)

data Module
  = Module Id     -- module name
           [Port] -- ports
           Stmt   -- module body
  | ExtModule Id     -- module name
              [Port] -- port
  deriving (Eq, Show)

data Port = Port Id       -- port name
                 RTy
  deriving (Eq, Show)

moduleName :: Module -> Id
moduleName (Module name _ _) = name
moduleName (ExtModule name _) = name
