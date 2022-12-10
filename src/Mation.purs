
-- | Mation is a frontend framework for Purescript.
-- | See [the Github repo](https://github.com/quelklef/mation)
-- |
-- | This module is entirely a convenience module, containing exports from other modules

module Mation (module X) where
  
import Mation.Core.Html (Html, enroot) as X
import Mation.Core.Dom (DomNode, DomEvent) as X
import Mation.Core.Mation (Mation, mkCont, mkEff, mkPure, mkNoop, mkStaged) as X
import Mation.Core.Run (runApp, runApp', onBody, underBody, onHtml) as X
