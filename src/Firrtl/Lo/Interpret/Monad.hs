module Firrtl.Lo.Interpret.Monad
  ( Interpret (..)
  -- re-exports
  , MonadReader (..)
  , MonadState (..)
  ) where

import           Control.Monad.Reader       (MonadReader (..))
import           Control.Monad.State        (MonadState (..))
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State  (State)

import           Firrtl.Lo.TypeCheck.Monad (Context)
import           Firrtl.Lo.Interpret.Value

newtype Interpret a =
  Interpret
  { runInterpret :: ReaderT (Context Value)  -- ^ inputs, read-only
                    (State  (Context Value)) -- ^ outputs, read/write
                    a
  } deriving
      ( Functor
      , Applicative
      , Monad
      )

deriving instance (EnvType Interpret ~ Context Value) => MonadReader Interpret
deriving instance (StateType Interpret ~ Context Value) => MonadState Interpret
