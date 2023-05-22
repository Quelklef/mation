module Mation.Elems.Prune where

import Mation.Core.Prelude

import Mation.Core.Html as Html
import Mation.Core.Html (Html)
import Mation.Core.Util.UnsureEq (class UnsureEq)
import Mation.Core.Util.FreeMonoid as FM


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
prune :: forall m p s. UnsureEq p => String -> p -> (p -> Html m s) -> Html m s
prune key param render = Html.mkPrune key render param

-- | Attaches a pruning key to a node but does not actually perform pruning
key :: forall m s. String -> Html m s -> Html m s
key = FM.map <<< Html.pruneScope

-- FIXME: add 'unPrune' (undoes `prune`) and 'pruneKeyed' (adds prune keys but does not prune)

