{-# language StandaloneDeriving, UndecidableInstances #-}
module Firrtl.Lo.TypeCheck.Monad where

import           Control.Monad.Except       (MonadError (..))
import           Control.Monad.Reader       (MonadReader (..))
import           Control.Monad.Identity     (Identity (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Semigroup      (Semigroup (..))

import qualified Numeric.Natural     as N

import Firrtl.Lo.Syntax
import Firrtl.Lo.TypeCheck.Types

data Error
  = ExpectedGround    Type
  | ExpectedGroundInt Type
  | ExpectedParameter Type
  | Mismatch Type Type
  | NotInScope Id (Maybe Type)
  | NoTopModule Id
  | ParameterToBig N.Natural Type
  | Connectable ConnType ConnType
  | Containable ConnType ConnType
  | Equivalent ConnType ConnType
  deriving Show

newtype Context = Ctx { unCtx :: HashMap Id ConnType }

instance Semigroup Context where
  (<>) (Ctx cl) (Ctx cr) = Ctx $ Map.union cl cr

instance Monoid Context where
  mappend = (<>)
  mempty  = Ctx Map.empty

singleton :: Id -> ConnType -> Context
singleton ident = Ctx . Map.singleton ident

insert :: Id -> ConnType -> Context -> Context
insert ident t = Ctx . Map.insert ident t . unCtx

lookup :: Id -> Context -> Maybe ConnType
lookup ident = Map.lookup ident . unCtx

newtype Check a = Check { runCheck :: ExceptT Error (ReaderT Context Identity) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    )

deriving instance (ErrorType Check ~ Error) => MonadError Check
deriving instance (EnvType Check ~ Context) => MonadReader Check
