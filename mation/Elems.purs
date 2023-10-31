
-- | Functions for creating `Html` values

module Mation.Elems (module X, module Mation.Elems) where

import Mation.Gen.Tags as X
import Mation.Core.Html (Html (..), hoist) as X
import Mation.Elems.Prune (prune) as X

import Mation.Core.Prelude

import Mation.Core.Html as Html
import Mation.Core.Html (Html)
import Mation.Core.Prop (Prop)
import Mation.Core.Prop as Prop
import Mation.Core.Dom (DomNode)


type Html' s = Html Effect s

-- | Create a text node
text :: forall m s. String -> Html m s
text = Html.mkText

-- | Embed a DOM node
rawNode :: forall m s. DomNode -> Html m s
rawNode = Html.mkRawNode

-- | Embed some raw html
rawHtml :: forall m s. String -> Html m s
rawHtml = Html.mkRawHtml

-- | Allows an `Html` to retrieve its capabilities outside of an event listner
withK :: forall m k. (k -> Html m k) -> Html m k
withK = Html.mkWithK

-- | Construct an element directly from a tag name, props, and children.
-- |
-- | This should only rarely be necessary
mkElem :: forall m s. String -> Array (Prop m s) -> Array (Html m s) -> Html m s
mkElem = Prop.mkTagFromProps

