
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
  , focusReadWriteL
  , hoistReadWriteL
  , ReadWrite
  , ReadWrite'
  , focusReadWrite
  , hoistReadWrite
  , Modify
  , Modify'
  , focusModify
  , hoistModify
  , Write
  , Write'
  , focusWrite
  , hoistWrite
  , ReadL
  , ReadL'
  , focusReadL
  , hoistReadL
  , Read
  , Read'
  , focusRead
  , hoistRead
  , read
  , write
  , modify
  , class CanFocusWithLens
  , focusWithLens
  , class CanFocusWithSetter
  , focusWithSetter
  , class CanFocusWithGetter
  , focusWithGetter
  ) as X

