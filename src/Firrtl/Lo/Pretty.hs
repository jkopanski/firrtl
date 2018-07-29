module Firrtl.Lo.Pretty
  ( terminal
  ) where

import           Firrtl.Lo.Pretty.Common
import qualified Firrtl.Lo.Pretty.Circuit      as Circuit
import qualified Firrtl.Lo.Syntax.Safe.Circuit as Safe

import           Data.Text.Lazy                            (Text)
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal

terminal :: Safe.Circuit -> Text
terminal = Terminal.renderLazy
         . fmap annToAnsiStyle
         . Pretty.layoutSmart options
         . Circuit.prettyCircuit
  where
    options = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }
