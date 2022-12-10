
-- | Functions for creating `Html` values

module Mation.Elems (module X, module Mation.Elems) where

import Mation.Gen.Tags as X
import Mation.Core.Html (Html (..)) as X

import Mation.Core.Prelude
import Effect (Effect)
import Data.Map as Map
import Data.Map (Map)

import Mation.Core.Html as Html
import Mation.Core.Html (Html)
import Mation.Core.Mation (Mation)
import Mation.Core.Dom (DomNode, DomEvent)
import Mation.Core.Prop (Prop)
import Mation.Core.Prop as Prop
import Mation.Core.Util.Assoc (Assoc (..))
import Mation.Core.Util.FreeMonoid as FM
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
enroot :: forall m large small. Lens' large small -> Html m small -> Html m large
enroot = Html.enroot

-- | A call to `prune` represents a request for the runtime to skip re-rendering
-- | a part of an application when the relevant part of the model doesn't change.
-- |
-- | Please **note that `prune` is not an entirely safe operation**; in specific
-- | circumstances `prune` will exhibit undesirable behaviour. Using `prune` in a
-- | way that *guarantees* correctness is subtle. Thankfully, it is also usually
-- | not necessary: in most applications, `prune` will "just work" with minimal
-- | care.
-- |
-- | Basic usage of `prune` is simple: in callsites where you are generating
-- | an `Html` value like
-- |
-- | ```
-- | html = f x
-- | ```
-- |
-- | You can replace this with
-- |
-- | ```
-- | html = prune Nothing f x
-- | ```
-- |
-- | And when the application is re-rendered, `html` will be skipped if `x` has not
-- | changed since the previous render.
-- |
-- | That means that **for `prune` to work correctly, `f` must never change**. For
-- | instance, this is not acceptable:
-- |
-- | ```
-- | html = prune Nothing (mkHtml a b) c
-- | ```
-- |
-- | and must instead be written as:
-- |
-- | ```
-- | html = prune Nothing (\(a /\ b /\ c) -> mkHtml a b c) (a /\ b /\ c)
-- | ```
-- |
-- | Here ends the documentation for basic `prune` usage. The rest of this text
-- | talks about the edge-cases of `prune` and how to avoid them.
-- |
-- | ***
-- |
-- | The way that `prune` works is as follows.
-- |
-- | Say the application is performing a
-- | re-render and is trying to diff two `Html` values which have both been `prune`d.
-- | Let one be the result of `prune f x` and the other the result of `prune f' x'`.
-- |
-- | In order to perform that diff, the runtime will compare the values `x` and `x'`.
-- | If they are equal then, since we've required of the user that that `f` not change,
-- | we know that `f x` and `f' x'` are the same. Hence, no diff need be performed.
-- |
-- | Even though we require of the user that the function given to `prune` never change,
-- | this strategy can actually still fail. Say, for instance, that our model contains,
-- | among other things, values of type `A` and `B`. And say we render our model to
-- | a `<div>` which contains two children, one a `<span>` containing the render
-- | for the `A` value, and one a `<span>` containing the render for the `B` value. And
-- | say that we `prune` both of these renders, and say that on one frame we choose
-- | to swap the order of the `<span>`s within their parent `<div>`.
-- |
-- | Then when we try to render, the `prune`d node for the `A` value will be matched up
-- | with the `prune`d node for the `B` value. In short, this will trip up the runtime
-- | and cause improper rendering. (And at the time of writing can even cause a
-- | Javascript exception! This is a known issue -- FIXME)
-- |
-- | This is an extremely unlikely scenario but not impossible. In case you need,
-- | for whatever reason, to insure against the possibility that this happens, you
-- | can pass a non-`Nothing` value as the first argument to `prune`. These values (when
-- | not `Nothing`) will be used as "keys" for the pruned node, providing them with
-- | a notion of identity: when diffing two pruned nodes, if they have different
-- | keys, the runtime will immediately know not to skip the diff.
prune :: forall m p s. UnsureEq p => Maybe String -> (p -> Html m s) -> p -> Html m s
prune mKey render param = Html.mkPrune mKey render param

