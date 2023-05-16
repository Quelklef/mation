
-- | Functions for creating and modifying `Prop`erties

module Mation.Props
  ( module X
  , style'
  , onInput'
  , fixup
  , mkPair
  , mkListener
  , dataset
  , remark
  , showUpdates
  ) where

import Data.Map (Map)
import Data.Map as Map
  
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
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Assoc as Assoc


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


-- | Set `data-` attributes on an element
dataset :: forall m s. Map String String -> Prop m s
dataset kvs = fixup $ dataset_f (kvs # Map.toUnfoldable # asArray # Assoc.fromFoldable)
  where
  asArray :: forall x. Array x -> Array x
  asArray = identity

foreign import dataset_f :: Assoc String String -> DomNode -> Effect { restore :: Effect Unit }


-- | Adds an arbitrary string to the `data-remark` attribute of an element
-- |
-- | Since mation mostly obviates the need for adding classes to elements,
-- | output HTML often ends up containing very little in the way of telling
-- | what an element *is*. Is this `<div class="mation-style-r93q9e">` my
-- | nav-bar? A button? An image wrapper? Who knows.
-- |
-- | Use of `remark` obviates this issue by allowing you to "label" elements
-- | with such semantic information. Consider using `remark` to label salient
-- | points in the DOM tree such as the outermost node of components.
remark :: forall m s. String -> Prop m s
remark rk = dataset (Map.singleton "remark" rk)


-- | Gives the node a red border whenever they are updated
-- |
-- | This is intended for debugging only!
showUpdates :: forall m s. Prop m s
showUpdates = fixup showUpdates_f

foreign import showUpdates_f :: DomNode -> Effect { restore :: Effect Unit }
