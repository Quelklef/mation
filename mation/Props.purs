
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
  , fixup
  , fixupM
  , fixupEUnutterable
  , fixupMUnutterable
  , mkPair
  , mkListener
  , remark
  , showUpdates
  , onClickElsewhere
  ) where

import Mation.Core.Prop (Prop, enroot, hoist) as X
import Mation.Gen.Attributes hiding (style, class_, data_) as X
import Mation.Gen.Events as X

import Prim.TypeError (class Warn, Text)
import Data.Map as Map
import Data.Array as Array

import Mation.Core.Prelude
import Mation.Core.Mation (Mation)
import Mation.Core.MationT (Step, MationT (..))
import Mation.Core.MationT as MationT
import Mation.Core.Prop (Prop)
import Mation.Core.Prop as Prop
import Mation.Core.Dom (DomEvent, DomNode)
import Mation.Core.Style (Style)
import Mation.Core.Style as Style
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Assoc as Assoc
import Mation.Core.Util.Revertible as Rev
import Mation.Gen.Attributes as Attr


-- | Override the `style` attribute of a node
-- |
-- | You probably want `addStyles` instead, which adds styles instead
-- | of overriding them. If you *want* to override,
-- | use `Mation.Gen.Attributes (style)`.
style :: forall m s.
  Warn (Text "Instead of Mation.Props (style), prefer Mation.Props (addCss) or Mation.Props (addStyles) or Mation.Gen.Attributes (style)") =>
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
  Warn (Text "Instead of Mation.Props (class_), prefer Mation.Props (addClasses) or Mation.Gen.Attributes (class_)") =>
  String -> Prop m s
class_ = Attr.class_

-- | Add a class to a node
-- |
-- | ***
-- |
-- | Prefer thise over `Mation.Gen.Attributes (class_)`, which will override existing classes
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
  Warn (Text "Instead of Mation.Props (data_), prefer Mation.Props (addDataset) or Mation.Gen.Attributes (data_)") =>
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

-- | Create a `Prop` which will execute a given function on the rendered DOM node
-- | and then execute the given `restore` before rendering the next frame
fixup :: forall m s. (DomNode -> Effect { restore :: Effect Unit }) -> Prop m s
fixup f = Prop.mkFixup \node -> Rev.mkRevertibleE (_.restore <$> f node)

-- | Like `fixup` but lives in `m`
fixupM :: forall m s. Functor m => (DomNode -> m { restore :: m Unit }) -> Prop m s
fixupM f =
  Prop.mkFixup \node -> Rev.mkRevertibleM $ MationT \_step ->
    (MationT.lift <<< _.restore) <$> f node

-- | Like `fixup` but with access the application `Step s`
-- |
-- | This is very powerful and is generally not needed.
-- |
-- | Apologies for the `MonadEffect` constraint
fixupEUnutterable :: forall m s. MonadEffect m => (DomNode -> Step s -> Effect { restore :: Effect Unit }) -> Prop m s
fixupEUnutterable f = fixupMUnutterable (\node step -> f node step # liftEffect # map (_restore %~ liftEffect))
  where _restore = prop (Proxy :: Proxy "restore")

-- | Like `fixup` lives in `m` and the application `Step s`
-- |
-- | This is very powerful and is generally not needed.
fixupMUnutterable :: forall m s. Functor m => (DomNode -> Step s -> m { restore :: m Unit }) -> Prop m s
fixupMUnutterable f =
  Prop.mkFixup \node -> Rev.mkRevertibleM $ MationT \step ->
    (MationT.lift <<< _.restore) <$> f node step

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


-- | Gives the node a red border whenever they are updated
-- |
-- | This is intended for debugging only!
showUpdates :: forall m s. Prop m s
showUpdates = fixup showUpdates_f

foreign import showUpdates_f :: DomNode -> Effect { restore :: Effect Unit }


-- | Fires when any node is clicked *except* for the target node (or a descendant of it)
onClickElsewhere :: forall m s. MonadUnliftEffect m => (DomEvent -> Mation m s) -> Prop m s
onClickElsewhere f =
  fixupMUnutterable \node step ->
    withRunInEffect \(toEffect :: m ~> Effect) -> do
      node # onClickElsewhere_f (\evt -> f evt step # toEffect) # liftEffect # map (_restore %~ liftEffect)

  where _restore = prop (Proxy :: Proxy "restore")

foreign import onClickElsewhere_f :: (DomEvent -> Effect Unit) -> DomNode -> Effect { restore :: Effect Unit }
