
-- | Functions for creating and modifying `Prop`erties

module Mation.Props
  ( module X
  , style
  , addCss
  , addStyles
  , class_
  , addClass
  , addClasses
  , data_
  , addDataset
  , onInputValue
  , remark
  , showUpdates
  , onClickElsewhere
  , rawAttribute
  , rawListener
  ) where

import Mation.Core.Prop (Prop, enroot, hoist) as X
import Mation.Gen.Attributes hiding (style, class_, data_) as X
import Mation.Gen.Events as X

import Prim.TypeError (class Warn, Text)
import Data.Map as Map
import Data.Array as Array

import Mation.Core.Prelude
import Mation.Core.Mation (Mation)
import Mation.Core.Prop (Prop)
import Mation.Core.Prop as Prop
import Mation.Core.Dom (DomEvent, DomNode)
import Mation.Core.Style (Style)
import Mation.Core.Style as Style
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Assoc as Assoc
import Mation.Gen.Attributes as Attr
import Mation.Props.Unsafe (fixup, fixupM')


-- | Override the `style` attribute of a node
-- |
-- | You probably want `addStyles` instead, which adds styles instead
-- | of overriding them. If you *want* to override,
-- | use `Mation.Gen.Attributes (style)`.
style :: forall m s.
  Warn (Text "Instead of Mation.Props (style), prefer Mation.Props (addCss) or Mation.Props (addStyles); or, explicitly import Mation.Gen.Attributes (style)") =>
  String -> Prop m s
style = Attr.style

-- | Add some inline CSS to a node
addCss :: forall m s. String -> Prop m s
addCss css = Style.toProp [Style.mkStyle css]

-- | Add some styles to a node
addStyles :: forall m s. Array Style -> Prop m s
addStyles = Style.toProp


-- | Override the `class` attribute of a node
-- |
-- | You probably want `addClass` or `addClasses` instead, which
-- | add class(es) instead of overriding them. If you *want* to override,
-- | use `Mation.Gen.Attributes (class_)`.
class_ :: forall m s.
  Warn (Text "Instead of Mation.Props (class_), prefer Mation.Props (addClasses); or, explicitly import Mation.Gen.Attributes (class_)") =>
  String -> Prop m s
class_ = Attr.class_

-- | Add a class to a node
-- |
-- | ***
-- |
-- | Prefer this over `Mation.Gen.Attributes (class_)`, which will override existing classes
addClass :: forall m s. String -> Prop m s
addClass = Array.singleton >>> addClasses

-- | Add multiple classes to a node
addClasses :: forall m s. Array String -> Prop m s
addClasses classes = fixup (addClasses_f classes)

foreign import addClasses_f :: Array String -> (DomNode -> Effect { restore :: Effect Unit })


-- | Override the `data` attribute of a node
-- |
-- | You probably want `addDataset` instead, which adds
-- | to the node dataset instead of overriding it. If you *want* to
-- | override, use `Mation.Gen.Attributes (data_)`.
data_ :: forall m s.
  Warn (Text "Instead of Mation.Props (data_), prefer Mation.Props (addDataset); or, explicitly import Mation.Gen.Attributes (data_)") =>
  String -> Prop m s
data_ = Attr.data_

-- | Add `data-` attributes to an element
addDataset :: forall m s. Map String String -> Prop m s
addDataset kvs = fixup $ addDataset_f (kvs # Map.toUnfoldable # asArray # Assoc.fromFoldable)
  where
  asArray :: forall x. Array x -> Array x
  asArray = identity

foreign import addDataset_f :: Assoc String String -> DomNode -> Effect { restore :: Effect Unit }


-- | Attach a listener to the `input` event.
-- |
-- | This is like `onInput` but retrieves the event value on your behalf
onInputValue :: forall m s. (String -> Mation m s) -> Prop m s
onInputValue f = X.onInput (\ev -> f (getTargetValue ev))

foreign import getTargetValue :: DomEvent -> String


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
remark rk = addDataset (Map.singleton "remark" rk)


-- | Prevents a node from being patched.
-- |
-- | This can also be achieved by setting the data
-- | attribute `mation-no-patch` to `true`
-- |
-- | Typically  patch-prevention will be initiated
-- | by *third-party* (ie, non-Mation) code, which will
-- | have no notion of a `Prop` and therefore want to
-- | use datasets rather than `dontPatch`.
-- | However, this `Prop` can be handy to easily prevent
-- | the document `<head>` from being
-- | patched (if you're using on `onHtml`)
dontPatch :: forall m s. Prop m s
dontPatch = addDataset (Map.singleton "mation-no-patch" "true")


-- | Gives the node a red border whenever it is updated
-- |
-- | This is intended for debugging only!
showUpdates :: forall m s. Prop m s
showUpdates = fixup showUpdates_f

foreign import showUpdates_f :: DomNode -> Effect { restore :: Effect Unit }


-- | Fires when any node is clicked *except* for the target node (or a descendant of it)
onClickElsewhere :: forall m s. MonadUnliftEffect m => (DomEvent -> Mation m s) -> Prop m s
onClickElsewhere f =
  fixupM' \node step ->
    withRunInEffect \(toEffect :: m ~> Effect) -> do
      node # onClickElsewhere_f (\evt -> f evt step # toEffect) # liftEffect # map (_restore %~ liftEffect)

  where _restore = prop (Proxy :: Proxy "restore")

foreign import onClickElsewhere_f :: (DomEvent -> Effect Unit) -> DomNode -> Effect { restore :: Effect Unit }


-- | Create a `Prop` directly from an HTML attribute key/value pair
-- |
-- | This should only rarely be necessary
rawAttribute :: forall m s. String -> String -> Prop m s
rawAttribute = Prop.mkPair

-- | Create a `Prop` directly from an event listener
-- |
-- | This should only rarely be necessary
rawListener :: forall m s. String -> (DomEvent -> Mation m s) -> Prop m s
rawListener = Prop.mkListener

