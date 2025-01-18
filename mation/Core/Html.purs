
-- | Mation virtual dom

module Mation.Core.Html where

import Mation.Core.Prelude

import Mation.Lenses (field)
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Assoc as Assoc
import Mation.Core.Util.Revertible (Revertible)
import Mation.Core.Util.Revertible as Rev
import Mation.Core.Dom (DomNode, DomEvent)
import Mation.Core.Util.FreeMonoid (class FreeMonoid)
import Mation.Core.Util.FreeMonoid as FM
import Mation.Core.Util.UnsureEq (Unsure)
import Mation.Core.Util.Exists (Exists, mkExists, mapExists, runExists)


-- | A single Virtual Node
-- |
-- | `Html` will be the free monoid over `VNode`, so one might
-- | also name this type `Html1`.
data VNode m k

    -- | Embed an existing DomNode directly into the virtual dom
  = VRawNode DomNode

    -- | Raw HTML
  | VRawHtml String

    -- | Text node
  | VText String

    -- | Tag node
  | VTag

      -- | Tag name
      { tag :: String

      -- | Html attributes
      , attrs :: Assoc String String

      -- | Event listeners
      , listeners :: Assoc String (DomEvent -> k -> m Unit)

      -- | Arbitrary revertible change on the mounted `DomNode`,
      -- | such as adding a class. The change is applied on each
      -- | render and reverted immediately before the next render.
      -- | (This is actually not quite accurate but is a fine
      -- | mental model to have)
      -- |
      -- | Fixups give a highly generic type of VNode "property".
      -- |
      -- | In principle having `fixup` subsumes the need for
      -- | both `attrs` and `listeners`, since one can always create
      -- | a `fixup` which adds an attribute or attaches a listener.
      -- |
      -- | ***
      -- |
      -- | Remark: it's worth asking why the `fixup` lives in `m`.
      -- | Since it mutates the `DomNode` should it not live in
      -- | something like `Effect` instead?
      -- |
      -- | Well, in the end we'll be using the `VNode` type
      -- | instantiated with some user-defined `m`.
      -- | We expect `m` to instantiate `MonadEffect` since it
      -- | has to be able to execute the Mation runtime (see the
      -- | comment on `Mation.Core.Mation (Mation)`), meaning
      -- | that working in `m` does give us our desired access
      -- | to `Effect`.
      , fixup :: DomNode -> k -> Revertible m

      -- | Children virtual nodes
      , children :: Array (VNode m k)
      }

  | VWithK (k -> VNode m k)

    -- | A node can be "pruned" by supplying, instead of an actual virtual node,
    -- | a value `params :: p` (for some `p`) plus a function `render : p -> VNode m`.
    -- | Together this pair forms a *deferred* computation of a virtual node.
    -- |
    -- | To prune, one must also supply an `UnsureEq` instance for `p` and
    -- | a so-called "key path" of type `Array String`. This allows for caching.
    -- |
    -- | Approximately speaking, we store a mapping from key paths to prune nodes
    -- | between frames, and we use the `UnsureEq` instance to detect when a `params`
    -- | value has changed between frames. When it hasn't, we don't bother to
    -- | compute `render params` at all -- we just re-use the node from the
    -- | previous frame.
    -- |
    -- | For this all to work, it must be ensured that for two adjacent render
    -- | frames, if a key path appears in both frames, then its parameter
    -- | type `p`, `UnsureEq p` instance, and render function `render` are equal
    -- | between the frames.
  | VPrune (Exists (PruneE m k))


newtype PruneE m k p = PruneE
  { keyPath :: Array String
  , params :: p
  , compare :: p -> p -> Unsure Boolean
      -- ^ Typeclass instance. Manually managed because Purescript doesn't have
      --   native support for existentials (not to mention constrained existentials)
  , render :: p -> VNode m k
  }


-- | Contravariantly map over a `VNode`
instance Contravariant (VNode m) where
  cmap f = case _ of
    VRawNode x -> VRawNode x
    VRawHtml x -> VRawHtml x
    VText x -> VText x
    VTag { tag, attrs, listeners, fixup, children } ->
      VTag { tag, attrs
           , listeners: listeners # (map <<< map) (f >>> _)
           , fixup: fixup # map (f >>> _)
           , children: children # map (cmap f)
           }
    VWithK withK -> VWithK (f >>> withK >>> cmap f)
    VPrune e -> VPrune $ e # mapExists \(PruneE { keyPath, params, compare, render }) ->
      PruneE { keyPath, params, compare, render: render >>> cmap f }


-- | Change the underlying monad of a virtual node
hoist1 :: forall m n k. Functor n => (m ~> n) -> VNode m k -> VNode n k
hoist1 f = case _ of
  VRawNode x -> VRawNode x
  VRawHtml x -> VRawHtml x
  VText x -> VText x
  VTag { tag, attrs, listeners, fixup, children } ->
    VTag { tag, attrs
         , listeners: listeners # (map <<< map <<< map) f
         , fixup: fixup # (map <<< map) (Rev.hoist f)
         , children: children # map (hoist1 f)
         }
  VWithK withK -> VWithK (hoist1 f <$> withK)
  VPrune e -> VPrune $ e # mapExists \(PruneE { keyPath, params, compare, render }) ->
    PruneE { keyPath, params, compare, render: render >>> hoist1 f }

  where _restore = field @"restore"



-- | Fix the `k` value of a `VNode`
-- |
-- | Once this is performed, every `cmap` operation is trivial since it asks
-- | for a function `a -> Unit`
fixVNode :: forall m k. k -> VNode m k -> VNode m Unit
fixVNode = cfix
  where

  cfix :: forall f a. Contravariant f => a -> f a -> f Unit
  cfix a0 = cmap (const a0)


-- | Prepend a key to pruning paths of all pruned descendants (including self)
addPruneKey :: forall m k. String -> VNode m k -> VNode m k
addPruneKey key = case _ of
  VRawNode x -> VRawNode x
  VRawHtml x -> VRawHtml x
  VText x -> VText x
  VTag { tag, attrs, listeners, fixup, children } ->
    VTag { tag, attrs, fixup, listeners
         , children: children # map (addPruneKey key)
         }
  VWithK withK -> VWithK (addPruneKey key <$> withK)
  VPrune e -> VPrune $ e # mapExists \(PruneE { keyPath, params, compare, render }) ->
    PruneE { params, compare
           , keyPath: [key] <> keyPath
           , render: render >>> addPruneKey key
           }

-- | Remove all contained `VPrune` nodes, collapsing their deferred
-- | computation.
-- |
-- | Preserves the container `<span style="display: contents">` in order to
-- | minimize difference between a VDom before and after being unpruned
unPrune1 :: forall m k. VNode m k -> VNode m k
unPrune1 = case _ of
  VRawNode x -> VRawNode x
  VRawHtml x -> VRawHtml x
  VText x -> VText x
  VTag { tag, attrs, listeners, fixup, children } ->
    VTag { tag, attrs, listeners, fixup
         , children: children # map unPrune1
         }
  VWithK withK -> VWithK (unPrune1 <$> withK)
  VPrune e ->
    e # runExists \(PruneE { params, render }) ->
          withWrapper [render params]


-- FIXME: VPrune and VWithK nodes get rendered as a <span style="display: contents">
--        over their children. This (A) is necessary to account for the fact
--        that Html is monoidal but VNode is not (try writing mkWithK without
--        using withWrapper); and (B) makes diffing easier for VPrune nodes.
--        But it's fundamentally a hack that needs fixing.
--        Note that the diffing algorithm directly uses knowledge of this hack.
withWrapper :: forall m k. Array (VNode m k) -> VNode m k
withWrapper children = VTag
  { tag: "span"
  , attrs: Assoc.fromFoldable [ "style" /\ "display: contents" ]
  , listeners: Assoc.fromFoldable []
  , fixup: mempty
  , children
  }



-- | Virtual DOM type
-- |
-- | An `Html m k` is, roughly, a tree of HTML tags annotated with event
-- | listeners of type `k -> m Unit`. That is, an event listener is
-- | an action in `m` with access to a value of type `k`, called
-- | its *capabilities*.
-- |
-- | In practice `k` is usually instantiated to something like
-- |
-- | ```purs
-- | { appConfig :: Read AppConfig, componentState :: ReadWrite Int }
-- | ```
-- |
-- | Indicating that the `Html` value's event listeners need readonly
-- | access to the application config and read/write access to an integer.
-- |
-- | This type is a `Monoid`, which has two important implications:
-- |
-- | - Every `Html` value is actually a so-called "fragment", meaning
-- |   that it can consist of more than one adjacent elements. For example,
-- |   just as there is an `Html` value representing `<b>text</b>`, there is
-- |   also one representing `<i>text</i><b>text</b>`
-- |
-- | - This type can be used with functions like `foldMap` and monoidal `when`,
-- |   which can be extremely convenient when constructing `Html` values
newtype Html m k = Html (Array (VNode m k))

instance FreeMonoid (Html m k) (VNode m k)

derive instance Newtype (Html m k) _
derive newtype instance Semigroup (Html m k)
derive newtype instance Monoid (Html m k)

instance Contravariant (Html m) where
  cmap f (Html arr) = Html (cmap f <$> arr)


-- | `Html` constructor
mkRawNode :: forall m k. DomNode -> Html m k
mkRawNode node = FM.singleton $ VRawNode node

-- | `Html` constructor
mkRawHtml :: forall m k. String -> Html m k
mkRawHtml html = FM.singleton $ VRawHtml html

-- | `Html` constructor
mkText :: forall m k. String -> Html m k
mkText text = FM.singleton $ VText text

-- | `Html` constructor
mkTag :: forall m k.
  { tag :: String
  , attrs :: Assoc String String
  , listeners :: Assoc String (DomEvent -> k -> m Unit)
  , fixup :: DomNode -> k -> Revertible m
  , children :: Array (Html m k)
  }
  -> Html m k
mkTag info = Html [ VTag info' ]
  where
  info' = info { children = FM.float info.children }

-- | `Html` constructor
mkWithK :: forall m k. (k -> Html m k) -> Html m k
mkWithK withK = Html [VWithK \k -> case withK k of Html ar -> withWrapper ar]

-- | `Html` constructor
mkPrune :: forall p m k. (p -> p -> Unsure Boolean) -> String -> (p -> Html m k) -> (p -> Html m k)
mkPrune compare key render params =
  FM.singleton $
    addPruneKey key $  -- Append key to key path of this node and all descendants
      VPrune $ mkExists $ PruneE
        { keyPath: []
        , params
        , compare
        , render: render >>> FM.unwrap >>> withWrapper
        }

-- | Remove pruning from all contained VNodes
unPrune :: forall m k. Html m k -> Html m k
unPrune = FM.map unPrune1

-- | Transform the underlying monad of an `Html`
-- |
-- | The given `m ~> n` is expected to be a monad morphism
hoist :: forall m n a. Functor n => (m ~> n) -> Html m a -> Html n a
hoist f (Html arr) = Html $ arr # map (hoist1 f)

