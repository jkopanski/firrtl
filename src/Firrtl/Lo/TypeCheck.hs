module Firrtl.Lo.TypeCheck
  ( module T
  , check
  ) where

import Control.Monad.Except        (runExceptT)
import Control.Monad.Reader        (runReaderT)
import Data.Functor.Identity       (runIdentity)
import Firrtl.Lo.Syntax            (Syntax)
import Firrtl.Lo.TypeCheck.Circuit (typecheck)
import Firrtl.Lo.TypeCheck.Monad as T
import Firrtl.Lo.TypeCheck.Types as T

check :: Syntax -> Either T.Error ()
check = runIdentity . flip (runReaderT . runExceptT . runCheck . typecheck) mempty
