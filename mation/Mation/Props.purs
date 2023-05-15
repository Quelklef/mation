
-- | Functions for creating and modifying `Prop`erties

module Mation.Props (module X, style', onInput', fixup, mkPair, mkListener, showUpdates) where
  
import Mation.Core.Prop (Prop, enroot, hoist) as X
import Mation.Gen.Attributes as X
import Mation.Gen.Events as X

import Mation.Core.Prelude
import Mation.Core.Mation (Mation)
import Mation.Core.Prop (Prop)
import Mation.Core.Prop as Prop
import Mation.Core.Dom (DomEvent, DomNode)
import Mation.Core.Style (Style)
import Mation.Core.Style as Style

-- | From a collection of `Style` values, produce a `Prop` for the `style` attribute on an `Html` node
style' :: forall m s. Array Style -> Prop m s
style' = Style.toProp

-- | Attach a listener to the `input` event.
-- |
-- | This differs from `onInput` in type: `onInput` provides a `DomEvent`, but this function provides the value `String`
onInput' :: forall m s. (String -> Mation m s) -> Prop m s
onInput' f = X.onInput (\ev -> f (getTargetValue ev))

foreign import getTargetValue :: DomEvent -> String

-- | Create a `Prop` which will execute a given function on the rendered DOM node
-- | and then execute the given `restore` before rendering the next frame
fixup :: forall m s. (DomNode -> Effect { restore :: Effect Unit }) -> Prop m s
fixup = Prop.mkFixup

-- | Create a `Prop` directly from an HTML attribute key/value pair
-- |
-- | Generally should not be necessary
mkPair :: forall m s. String -> String -> Prop m s
mkPair = Prop.mkPair

-- | Create a `Prop` directly from an event listener
-- |
-- | Generally should not be necessary
mkListener :: forall m s. String -> (DomEvent -> Mation m s) -> Prop m s
mkListener = Prop.mkListener


-- | Gives the node a red border whenever they are updated
-- |
-- | This is intended for debugging only!
showUpdates :: forall m s. Prop m s
showUpdates = fixup showUpdates_f

foreign import showUpdates_f :: DomNode -> Effect { restore :: Effect Unit }
