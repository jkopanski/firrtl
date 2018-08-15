module Firrtl.Lo.Interpret.Monad where

import           Control.Monad.Reader       (MonadReader (..))
import           Control.Monad.State        (MonadState (..), modify)
import           Control.Monad.Identity     (Identity (..))
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State  (StateT, State)

import qualified Firrtl.Lo.Syntax.Safe  as Safe
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

-- interpret
--   :: Context Int -- ^ module inputs
--   -> Safe.Module -- ^ module
--   -> Context Int -- ^ module outputs
-- interpret inputs m = undefined
