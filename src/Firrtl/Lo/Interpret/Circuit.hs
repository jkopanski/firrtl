module Firrtl.Lo.Interpret.Circuit where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List           (find)
import qualified Data.List.NonEmpty  as NE
import qualified Data.HashMap.Strict as Map

import Firrtl.Lo.Syntax.Common (Id)
import Firrtl.Lo.Syntax.Safe.Circuit
import Firrtl.Lo.Syntax.Safe.Stmt
import Firrtl.Lo.TypeCheck.Monad

import qualified Firrtl.Lo.Interpret.Stmt as Stmt
import           Firrtl.Lo.Interpret.Monad
import           Firrtl.Lo.Interpret.Value

interpret
  :: Module
  -> Context Value -- | inputs to a module
  -> Context Value -- | outputs
interpret (ExtModule _ _) _ = mempty -- | TODO: nothing to do here?

interpret (Module _ ports body) ctx = filterOutput (run body)
  where run :: Stmt -> Context Value
        run = flip execState mempty
            . flip runReaderT ctx
            . runInterpret
            . Stmt.interpret

        outputNames = (\(MkSomePort _ (Port name)) -> name) <$> ports

        isOutput :: Id -> Bool
        isOutput ident = ident `elem` outputNames

        filterOutput :: Context Value -> Context Value
        filterOutput (Ctx env) = Ctx (Map.filterWithKey (\k _ -> isOutput k) env)


simulate :: Circuit -> Context Value
simulate (Circuit ident mods) =
  let mtarget = find ((==) ident . mName) (NE.toList mods)
   in case mtarget of
        Nothing -> error "No top level module"
        Just t  -> interpret t mempty
