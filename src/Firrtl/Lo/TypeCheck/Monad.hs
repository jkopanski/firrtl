{-# language
        DataKinds
      , UndecidableInstances
      , TypeInType #-}
module Firrtl.Lo.TypeCheck.Monad where

import           Control.Monad              ((>=>))
import           Control.Monad.Except       (MonadError (..))
import           Control.Monad.State        (MonadState (..))
import           Control.Monad.Identity     (Identity (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.State  (StateT)

import           Data.Functor.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
-- import           Data.Kind (type (*))
import           Data.Semigroup      (Semigroup (..))
import           Data.Singletons
import           Data.Nat

import qualified Numeric.Natural     as N

import Firrtl.Lo.Syntax
import Firrtl.Lo.Syntax.Safe.Expr as Safe
import Firrtl.Lo.TypeCheck.Ty

data Error
  = ExpectedGround    Ty
  | ExpectedGroundInt Ty
  | ExpectedParameter Ty
  | Mismatch Ty Ty
  | NodeMale Ty
  | NotInScope Id (Maybe Ty)
  | NoTopModule Id
  | ParameterToBig N.Natural Ty
  | Connectable Ty Ty
  | Containable Ty Ty
  | Equivalent Ty Ty
  deriving Show

data Context =
  Ctx { nodes :: HashMap Id Safe.SomeExpr
      , wires :: HashMap Id Safe.SomeExpr
      }

instance Semigroup Context where
  (<>) cl cr = Ctx { nodes = Map.union (nodes cl) (nodes cr)
                   , wires = Map.union (wires cl) (wires cr)
                   }

instance Monoid Context where
  mappend = (<>)
  mempty  = Ctx Map.empty Map.empty

-- singleton :: Id -> Ty -> Context
-- singleton ident = Ctx . Map.singleton ident

insertNode :: (SingI s, SingI n) => Id -> Safe.Expr '(s, n, 'Male) -> Context -> Context
insertNode ident e ctx =
  ctx { nodes = Map.insert ident (fromExpr e) (nodes ctx) }

-- TODO: this could guarantee thet it is Safe.Expr '(s, n, 'Male)
lookupNode :: Id -> Context -> Maybe Safe.SomeExpr
lookupNode ident = Map.lookup ident . nodes

-- Should we go ReaderT and mutate it's context?
-- Check { runCheck :: ExceptT Error (ReaderT Context IO) a }
-- https://www.fpcomplete.com/blog/2017/07/the-rio-monad
newtype Check a =
  Check { runCheck :: ExceptT Error (StateT Context Identity) a }
    deriving
      ( Functor
      , Applicative
      , Monad
      )

deriving instance (ErrorType Check ~ Error) => MonadError Check
-- deriving instance (StateType Check ~ Context) => MonadState Check

instance MonadState Check where
  type StateType Check = Context
  get = get
  put = put

class Typed ast where
  -- type TypeFor ast
  type TypeSafe ast

--   hasType  :: TypeFor ast -> Check ()
--   typeOf   :: ast -> Check (TypeFor ast)
  typeSafe :: ast -> Check (TypeSafe ast)

cataM :: (Traversable (Base t), Monad m, Recursive t) => (Base t c -> m c) -> t -> m c
cataM = cata . (sequence >=>)
