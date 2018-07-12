{-# language
        DataKinds
      , TypeInType #-}
module Firrtl.Lo.Syntax.Safe.Circuit where

import Data.Kind          (type (*))
import Data.List.NonEmpty (NonEmpty)
import Data.Singletons

import Firrtl.Lo.Syntax.Common (Id)
import Firrtl.Lo.Syntax.Safe.Stmt
import Firrtl.Lo.TypeCheck.Ty

data Circuit :: * where
  Circuit :: Id -> NonEmpty Module -> Circuit
  
data Module :: * where
  Module    :: Id -> [SomePort] -> Stmt -> Module
  ExtModule :: Id -> [SomePort]         -> Module

mName :: Module -> Id
mName (Module n _ _)  = n
mName (ExtModule n _) = n

mPorts :: Module -> [SomePort]
mPorts (Module _ p _)  = p
mPorts (ExtModule _ p) = p

data Port :: Ty -> * where
  Port :: Id -> Port t

data SomePort :: * where
  MkSomePort :: Sing t -> Port t -> SomePort
