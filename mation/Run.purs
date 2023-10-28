module Mation.Run
  ( runApp
  , onBody
  , underBody
  , onHtml
  , module X
  ) where

import Mation.Core.Prelude

import Mation.Core.Run (Daemon, Daemon') as X

import Mation.Core.Html (Html)
import Mation.Core.Dom (DomNode)
import Mation.Core.Run (runAppM, Daemon)
import Mation.Core.Refs (ReadWrite)


-- | Run an mation application
-- |
-- | Accepts a record containing the following fields:
-- |
-- | - `initial :: s`
-- |
-- |   The initial model value. The type variable uses the character `s`
-- |   for "state"
-- |
-- | - `render :: s -> Html m s`
-- |
-- |   Specifies how to display the application
-- |
-- | - `daemon :: Daemon Effect s`
-- |
-- |   Possibly-long-lived process which has read/write access to
-- |   the application state and runs in parallel with the application
-- |   itself.
-- |
-- |   The daemon is the only thing that can read the state back
-- |   out of a running application.
-- |
-- |   If the daemon changes the application state, the application
-- |   will re-render.
-- |
-- |   An "empty" daemon is given by `\_ -> pure unit`
-- |
-- | - `root :: Effect DomNode`
-- |
-- |   Specifies where the application should be mounted. See `onBody`
-- |   and others below.
-- |
-- |   If you're not sure what to supply for this, use `underBody`
-- |
-- |   Remarks:
-- |
-- |   - The process of mounting may replace the given `DomNode`. The
-- |     given `DomNode` only guarantees *where* the mounting occurs in
-- |     the DOM
-- |
-- |   - This is an `Effect DomNode`, meaning that the mountpoint can
-- |     theoretically change between frames. If you do change the
-- |     mountpoint, be sure that the new one is equivalent to the old
-- |     one up to details produced by `render`. For various reasons,
-- |     the Mation rendering algorithm is sensitive to this.
-- |
-- | ***
-- |
-- | If you want to live in a monad other than `Effect`,
-- | see `Mation.Core.Run (runAppM)`. Using `runAppM` is not recommended,
-- | for reasons discussed in its documentation
runApp :: forall s.
  { initial :: s
  , render :: s -> Html Effect (ReadWrite Effect s)
  , root :: Effect DomNode
  , daemon :: Daemon Effect s
  } -> Effect Unit
runApp = runAppM


-- | Mount an application on `<body>`. The application will replace `<body>` each render
foreign import onBody :: Effect DomNode

-- | Mount an application as a child of `<body>`
foreign import underBody:: Effect DomNode

-- | Mount an application on `<html>`. The application will replace `<html>` each render
foreign import onHtml :: Effect DomNode

