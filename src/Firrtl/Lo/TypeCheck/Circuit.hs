module Firrtl.Lo.TypeCheck.Circuit where

import           Control.Monad.Except (throwError)
import           Control.Monad.Reader (local)
import qualified Data.List.NonEmpty   as NE

import Firrtl.Lo.Syntax
import Firrtl.Lo.TypeCheck.Monad
import Firrtl.Lo.TypeCheck.Stmt

typecheck :: Syntax -> Check ()
typecheck (Top (Circuit name modules)) = do
  let top = NE.filter (\m -> moduleName m == name) modules
  if length top /= 1
     then throwError $ NoTopModule name
     else mapM_ typecheckM modules

typecheckM :: Module -> Check ()
typecheckM (ExtModule _ _) = pure ()
typecheckM (Module _ ports body) =
  local (\ctx -> foldr (\(Port pname ptype) -> insert pname ptype) ctx ports) $
    check body
