module Mation.Elems.Prune where

import Mation.Core.Prelude

import Mation.Core.Html as Html
import Mation.Core.Html (Html)
import Mation.Core.Util.UnsureEq (class UnsureEq)
import Mation.Core.Util.FreeMonoid as FM


-- | Marks a node for "pruning", meaning that its `Html` will only be re-computed
-- | when it actually changes. This can have an enormous impact on the performance
-- | of a mation application.
-- |
-- | The way that pruning works is somewhat subtle and is not difficult to get
-- | wrong. However, there's a relatively easy pattern one can follow to perform
-- | pruning which should work fine in the majority of cases.
-- |
-- | Say you've identified some expensive part of a component view:
-- |
-- | ```
-- | view :: Model -> OtherStuff -> Html Effect (Model /\ OtherStuff)
-- | view model otherStuff =
-- |   let
-- |     expensive :: Html Effect (Model /\ OtherStuff)
-- |     expensive = someExpensiveComputation model otherStuff
-- |   in
-- |     useExpensive expensive model otherStuff
-- | ```
-- |
-- | You'd like to prune the `expensive` node in order to boost application
-- | performance. To do this, wrap it in a function, provide the function
-- | arguments to `prune`, and also provide some string to `prune`:
-- |
-- | ```
-- |     expensive :: Html Effect (Model /\ OtherStuff)
-- |     expensive =
-- |         prune "expensive" (model /\ otherStuff) \(model /\ otherStuff) ->
-- |             someExpensiveComputation model otherStuff
-- | ```
-- |
-- | The idea of pruning is to replace an actual virtual node with a pair
-- | of a function that can construct the node alongside inputs to that function.
-- | We track those inputs over time, and only actually recompute the
-- | virtual node when the inputs change.
-- |
-- | Accordingly, it is *crucial* that the function take as parameter *every*
-- | variable of the expensive computation.
-- |
-- | The string `"expensive"` is called the prune *key*. In most cases one should
-- | not have to worry about the choice of prune key beyond ensuring that each
-- | key is reasonably unique.
-- |
-- | Read on if you'd like more details on how pruning works...
-- |
-- | ***
-- |
-- | A pruned node consists of a "render" function producing a virtual node,
-- | a "parameter" value of type `p` (for some `p` instantiating `UnsureEq`),
-- | and a string "prune key".
-- |
-- | As discussed, the mation runtime holds onto prune parameters between
-- | renders, recomputing the virtual node with the given render function
-- | when the parameter changes (according to its `UnsureEq` instance).
-- |
-- | For this to work properly, the render function must not close over
-- | any variables. If it does, then changes to those variables will
-- | wrongly not invoke a re-render.
-- |
-- | The prune key is used to pair up pruned nodes in adjacent render
-- | frames. For each prune node we compute its "key path", which is the
-- | sequence of prune keys in the VDOM path from the VDOM root down to
-- | the node. When rendering a pruned node, we check to see if the last
-- | frame had any node with the same key path. If it did, we match them
-- | up for pruning and perform parameter comparison, re-using the DOM
-- | Node from the previous render if the render parameters have not changed.
-- |
-- | For this to work, one must ensure that, across adjacent renders,
-- | if two prune nodes have the same key path then they also have the
-- | same type `p` and render function `p -> Html m s`.
-- |
-- | ***
-- |
-- | One might wonder why we go through all this business with prune keys
-- | and key paths instead of just matching up pruned nodes based on position
-- | in the VDOM (which is how
-- | Elm's [Html.Lazy](https://package.elm-lang.org/packages/elm/html/1.0.0/Html-Lazy)
-- | [seems to do it](https://guide.elm-lang.org/optimization/lazy.html)).
-- |
-- | One trouble is that position-based matching is intolerant to position changes.
-- | If you were to move a position-pruned node around the VDOM, or add or remove
-- | a wrapper node around it, or add or remove a wrapper node around *any ancestor*,
-- | the node would unconditionally recompute, even if you haven't changed it.
-- |
-- | Another trouble is that position-based matching has to compare `render`
-- | functions, and function comparison has to be done with identity comparison.
-- | To avoid render functions comparing as inequal every frame we'd have to either
-- | lift the function definition to module-scope or try to guarantee that our
-- | purescript compiler performs scope lifitng of closed expressions. Both of these
-- | options are not particularly enticing.
-- |
-- | Finally I'd like to note about the use of *key paths* over just *keys*,
-- | which in principle would also work. The issue is that whatever we use to
-- | match up prune nodes needs to be *globally* unique. If we use plain keys, this
-- | would mean that every call to `prune` has to choose a *globally*-unique string.
-- | For library code this is plain impossible: who knows what strings the library
-- | client will use? (The library code could generate long random strings, I
-- | suppose, and have a near-guarantee of uniqueness, but that would be
-- | extremely ugly.)
-- |
-- | By using key *paths* to match pruned nodes, we only have to ensure some
-- | local structure in order to guarantee global uniqueness of key paths.
-- |
-- | For example, on way to guarantee unique key paths is to ensure that:
-- |
-- | - If a node is given a key, so are its siblings, and all these keys
-- |   are distinct
-- | - All parts of the VDOM with unknown keying are given prune keys
-- |
-- | This is not the only way to ensure globally-unique prune key paths. (In fact,
-- | this method is rather stringent -- but it should work!)
prune :: forall m p s. UnsureEq p => String -> p -> (p -> Html m s) -> Html m s
prune key param render = Html.mkPrune key render param

-- | Removes pruning from an `Html` value *and all descendants*
-- |
-- | This may be necessary in order to ensure validity of the prune tree when
-- | embedding some `Html` from an unknown source
unPrune :: forall m s. Html m s -> Html m s
unPrune = Html.unPrune

-- | Attaches a pruning key to a node but does not actually perform pruning
key :: forall m s. String -> Html m s -> Html m s
key = FM.map <<< Html.addPruneKey

