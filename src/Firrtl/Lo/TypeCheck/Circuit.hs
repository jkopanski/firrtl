{-# language
        DataKinds
      , TypeInType #-}
module Firrtl.Lo.TypeCheck.Circuit where

import Control.Monad.Except (throwError)
import qualified Data.List.NonEmpty as NE
import Data.Singletons.Prelude hiding (Error)
import Data.Nat

import qualified Firrtl.Lo.Syntax.Safe as Safe
import           Firrtl.Lo.Syntax.Circuit
import           Firrtl.Lo.Syntax.Stmt
import           Firrtl.Lo.TypeCheck.Monad
import           Firrtl.Lo.TypeCheck.Stmt
import           Firrtl.Lo.TypeCheck.Ty

instance Typed Circuit where
  type TypeSafe Circuit = Safe.Circuit

  typeSafe :: Circuit -> Check Safe.Circuit
  typeSafe (Circuit ident modules) = do
    let mainModules = NE.filter ((ident ==) . moduleName) modules
        numMains = length mainModules

    if numMains /= 1
       then throwError $ NoTopModule ident
       else Safe.Circuit ident <$> traverse typeSafe modules
  
instance Typed Module where
  type TypeSafe Module = Safe.Module

  typeSafe :: Module -> Check Safe.Module
  typeSafe (ExtModule ident ports) =
    Safe.ExtModule ident <$> (traverse typeSafe ports)

  typeSafe (Module ident ports body) = do
    Safe.Module ident <$> traverse typeSafe ports <*> typeSafe body

instance Typed Port where
  type TypeSafe Port = Safe.SomePort

  typeSafe :: Port -> Check Safe.SomePort
  typeSafe (Port ident (t, n, g)) =
    let mk = Safe.MkSomePort
     in pure $ case someNatVal n of
      SomeSing sn -> case (t, g) of
        (Clock   , Bi    ) -> mk (STuple3 SClock    sn SBi    ) (Safe.Port ident)
        (Clock   , Female) -> mk (STuple3 SClock    sn SFemale) (Safe.Port ident)
        (Clock   , Male  ) -> mk (STuple3 SClock    sn SMale  ) (Safe.Port ident)
        (Signed  , Bi    ) -> mk (STuple3 SSigned   sn SBi    ) (Safe.Port ident)
        (Signed  , Female) -> mk (STuple3 SSigned   sn SFemale) (Safe.Port ident)
        (Signed  , Male  ) -> mk (STuple3 SSigned   sn SMale  ) (Safe.Port ident)
        (Unsigned, Bi    ) -> mk (STuple3 SUnsigned sn SBi    ) (Safe.Port ident)
        (Unsigned, Female) -> mk (STuple3 SUnsigned sn SFemale) (Safe.Port ident)
        (Unsigned, Male  ) -> mk (STuple3 SUnsigned sn SMale  ) (Safe.Port ident)
