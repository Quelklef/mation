
-- | Functions for creating `Html` values

module Mation.Elems (module X, module Mation.Elems) where

import Mation.Gen.Tags as X
import Mation.Core.Html (Html (..)) as X

import Mation.Core.Prelude

import Mation.Core.Html as Html
import Mation.Core.Html (Html)
import Mation.Core.Dom (DomNode)
import Mation.Core.Prop (Prop)
import Mation.Core.Prop as Prop
import Mation.Core.Util.UnsureEq (class UnsureEq)


type Html' s = Html Effect s

-- | Create a text node
text :: forall m s. String -> Html m s
text = Html.mkText

-- | Embed raw html
rawHtml :: forall m s. String -> Html m s
rawHtml = Html.mkRawHtml

-- | Embed a DOM node
rawNode :: forall m s. DomNode -> Html m s
rawNode = Html.mkRawNode

-- | Bring-your-own tagname
mkTag :: forall m s. String -> Array (Prop m s) -> Array (Html m s) -> Html m s
mkTag = Prop.mkElement


-- | Embed one `Html` within another
enroot :: forall m large small. Setter' large small -> Html m small -> Html m large
enroot = Html.enroot

-- | Marks a node for "pruning", meaning that its `Html` will only be re-computed
-- | when its model actually changes.
-- |
-- | The `prune` function accepts three arguments: a parameter value of type `p`,
-- | a function `p -> Html m s` that can render it, and a so-called "key" of type
-- | `String`. Mation will hold onto the parameter value between frames and only
-- | recompute the `Html` value when the parameter changes.
-- |
-- | The runtime knows which parameter to look for based on the given key. It
-- | computes the so-called "key path" for the pruned node, which is the sequence
-- | of pruning keys starting at the VDOM root and going down to the pruned node.
-- | It uses this key path to find the parameter value from the previous frame.
-- |
-- | For pruning to work correctly, **the user must guarantee** that between two
-- | frames if two pruned nodes have the same key path then they also have
-- | the same type `p` and render function `p -> Html m s`.
prune :: forall m p s. UnsureEq p => String -> (p -> Html m s) -> p -> Html m s
prune key render param = Html.mkPrune key render param

