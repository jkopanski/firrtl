module Firrtl.Lo.Context
  ( Context (..)
  , singleton
  , insert
  , lookup
  , delete
  ) where

import Prelude hiding (lookup)

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Semigroup      (Semigroup (..))

import           Firrtl.Lo.Syntax    (Id)

-- | Context associate identifiers with type/value/whaterver
--   wich should be sufficient information
--   during typechecking phase
newtype Context t = Ctx { unCtx :: HashMap Id t }
  deriving (Functor, Traversable, Foldable, Semigroup, Monoid)

singleton :: Id -> t -> Context t
singleton ident = Ctx . Map.singleton ident

insert :: Id -> t -> Context t -> Context t
insert ident ty (Ctx m) = Ctx (Map.insert ident ty m)

lookup :: Id -> Context t -> Maybe t
lookup ident (Ctx m) = Map.lookup ident m

delete :: Id -> Context t -> Context t
delete ident (Ctx m) = Ctx (Map.delete ident m)
