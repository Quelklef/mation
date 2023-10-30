
-- | Top-level convenience module, containing only re-exports

module Mation (module X) where

import Data.Functor.Contravariant (cmap) as X
  
import Mation.Elems (Html, Html') as X
import Mation.Props (Prop, Prop') as X
import Mation.Styles (Style) as X
import Mation.Run (runApp, Daemon, Daemon', onBody, underBody, onHtml) as X
import Mation.Core.Dom (DomNode, DomEvent) as X
import Mation.Core.Refs
  ( ReadWriteL
  , ReadWriteL'
  , ReadWrite
  , ReadWrite'
  , Modify
  , Modify'
  , Write
  , Write'
  , ReadL
  , ReadL'
  , Read
  , Read'
  , class ReadRef
  , read
  , class WriteRef
  , write
  , class ModifyRef
  , modify
  , class FocusRefWithLens
  , focusWithLens
  , class FocusRefWithSetter
  , focusWithSetter
  , class FocusRefWithGetter
  , focusWithGetter
  ) as X

