
-- | Mation is a frontend framework for Purescript.
-- | See [the Github repo](https://github.com/quelklef/mation)
-- |
-- | This module is entirely a convenience module, containing exports from other modules

module Mation (module X) where
  
import Mation.Elems (Html, Html') as X
import Mation.Props (Prop, Prop') as X
import Mation.Styles (Style) as X
import Mation.Run (runApp, onBody, underBody, onHtml) as X
import Mation.Core.Daemon (Daemon, Daemon') as X
import Mation.Core.Dom (DomNode, DomEvent) as X
import Mation.Core.Mation (Mation, Mation', toBuffered) as X

