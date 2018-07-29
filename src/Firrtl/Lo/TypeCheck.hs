module Firrtl.Lo.TypeCheck
  ( module T
  , check
  ) where

import Control.Monad.Except      (runExceptT)
import Control.Monad.State       (evalStateT)
import Data.Functor.Identity     (runIdentity)
import           Firrtl.Lo.Syntax            (Circuit)
import qualified Firrtl.Lo.Syntax.Safe       as Safe
import           Firrtl.Lo.TypeCheck.Circuit as T
import           Firrtl.Lo.TypeCheck.Expr    as T
import           Firrtl.Lo.TypeCheck.Stmt    as T
import           Firrtl.Lo.TypeCheck.Monad   as T
import           Firrtl.Lo.TypeCheck.Ty      as T

check :: Circuit -> Either Error Safe.Circuit
check = runIdentity
      . flip ( evalStateT
             . runExceptT
             . runCheck
             ) mempty
      . typeSafe
