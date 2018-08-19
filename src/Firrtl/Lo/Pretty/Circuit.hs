{-# language TypeInType #-}
module Firrtl.Lo.Pretty.Circuit where

import qualified Data.List.NonEmpty as NE
import           Data.Singletons.Prelude.Tuple
import           Data.Text.Prettyprint.Doc (Doc, (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty

import           Firrtl.Lo.Pretty.Common
import qualified Firrtl.Lo.Pretty.Stmt as Stmt
import           Firrtl.Lo.Syntax.Safe.Circuit
import           Firrtl.Lo.TypeCheck.Ty

prettyCircuit :: Circuit -> Doc Ann
prettyCircuit (Circuit n modules) =
  keyword "circuit" <+> Pretty.pretty n <+> ":" <> Pretty.nest 2
    (Pretty.hardline <> (Pretty.vsep (NE.toList (prettyModule <$> modules))))

prettyModule :: Module -> Doc Ann
prettyModule (ExtModule n ports) =
  keyword "extmodule" <+> Pretty.pretty n <+> ":" <+> Pretty.nest 2
    (Pretty.hardline <> prettyPorts (prettyPort <$> ports))
prettyModule (Module n ports stmts) =
  keyword "module" <+> Pretty.pretty n <+> ":"
                   <+> Pretty.nest 2
                     (Pretty.hardline <> Pretty.align
                       (Pretty.vsep [ prettyPorts (prettyPort <$> ports)
                                    , Stmt.pretty stmts
                                    ]))
  
data PrettyPort = PPort
  { dir  :: Doc Ann -- ^ pretty gender (input/output/inout)
  , name :: Doc Ann -- ^ pretty identifier
  , ty   :: Doc Ann -- ^ pretty type definition
  }

prettyPorts :: [PrettyPort] -> Doc Ann
prettyPorts [] = mempty
prettyPorts ports =
  let fillDir = 7 -- 1 + length "output"
      fillName = maximum (length . show . name <$> ports)
      pport port = Pretty.fill fillDir (dir port)
               <+> Pretty.fill fillName (name port)
               <+> ":"
               <+> ty port
   in Pretty.align (Pretty.vcat (pport <$> ports))

prettyPort
  :: SomePort
  -> PrettyPort
prettyPort (MkSomePort portty@(STuple3 _ _ g) (Port ident)) =
  let direction = keyword $ case g of
        SBi     -> "inout"
        SFemale -> "output"
        SMale   -> "input"
      pid = Pretty.pretty ident 
      pt = prettyTy portty 
   in PPort direction pid pt
