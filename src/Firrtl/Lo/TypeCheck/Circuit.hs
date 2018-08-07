{-# language
        DataKinds
      , TypeInType #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Firrtl.Lo.TypeCheck.Circuit where

import Control.Monad.Except (throwError)
import qualified Data.List.NonEmpty as NE
import Data.Singletons.Prelude hiding (Error)
import Data.Width

import qualified Firrtl.Lo.Syntax.Safe as Safe
import           Firrtl.Lo.Syntax.Circuit
-- import           Firrtl.Lo.Syntax.Stmt
import           Firrtl.Lo.TypeCheck.Monad
import           Firrtl.Lo.TypeCheck.Stmt  ()
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
  typeSafe (ExtModule ident ps) =
    Safe.ExtModule ident <$> (traverse typeSafe ps)

  typeSafe (Module ident ps body) = do
    safePorts <- traverse typeSafe ps

    -- imitate Reader local
    save <- get
    modify (\s -> foldr addPort s safePorts)
    -- check module body
    safeBody <- typeSafe body
    -- restore the state
    -- note that we will not restore the env if we fail
    -- to typecheck, but that is not a problem since
    -- we want to crash anyway
    put save
    pure (Safe.Module ident safePorts safeBody)

      where addPort :: Safe.SomePort -> Context RTy -> Context RTy
            addPort (Safe.MkSomePort s (Safe.Port name)) ctx =
              insert name (FromSing s) ctx

instance Typed Port where
  type TypeSafe Port = Safe.SomePort

  typeSafe :: Port -> Check Safe.SomePort
  typeSafe (Port ident (t, n, g)) =
    let mk = Safe.MkSomePort
     in pure $ case toSing (Width n) of
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
