{-# language
        DataKinds
      , TypeInType #-}
module Firrtl.Lo.TypeCheck.Circuit where

import Data.Singletons.Prelude hiding (Error)
import Data.Nat

import qualified Firrtl.Lo.Syntax.Safe as Safe
import           Firrtl.Lo.Syntax.Circuit
import           Firrtl.Lo.Syntax.Stmt
import           Firrtl.Lo.TypeCheck.Monad
import           Firrtl.Lo.TypeCheck.Ty

instance Typed Circuit where
  type TypeSafe Circuit = Safe.Circuit

  typeSafe :: Circuit -> Check Safe.Circuit
  typeSafe = undefined
  
instance Typed Module where
  type TypeSafe Module = Safe.Module

  typeSafe :: Module -> Check Safe.Module
  typeSafe = undefined

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
