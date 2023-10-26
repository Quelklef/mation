
-- | Mation virtual dom

module Mation.Core.Html where
  
import Mation.Core.Prelude

import Mation.Lenses (field)
import Mation.Core.MationT (MationT)
import Mation.Core.MationT as MationT
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Assoc as Assoc
import Mation.Core.Util.Revertible (Revertible)
import Mation.Core.Util.Revertible as Rev
import Mation.Core.Dom (DomNode, DomEvent)
import Mation.Core.Util.FreeMonoid (class FreeMonoid)
import Mation.Core.Util.FreeMonoid as FM
import Mation.Core.Util.UnsureEq (class UnsureEq, unsureEq, Unsure)
import Mation.Core.Util.Exists (Exists, mkExists, mapExists, runExists)


-- | A single Virtual Node
-- |
-- | Parameterized by one type variable `m` which gives the monad
-- | in which event listeners and fixups live
-- |
-- | `Html` will be the free monoid over `VNode`, so one might
-- | also name this type `Html1`.
data VNode m

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
      , listeners :: Assoc String (DomEvent -> m Unit)

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
      -- | instantiated with `m ~ MationT s n` for some user-defined
      -- | `n`. We expect `n` to instantiate `MonadEffect` since it
      -- | has to be able to execute the Mation runtime (see the
      -- | comment on `Mation.Core.Mation (Mation)`), meaning
      -- | that working in `n` does give us our desired access
      -- | to `Effect`. Additionally, via `MationT s` it will
      -- | give us access to the application `Step s`, which is useful
      -- | for some `fixup`s.
      , fixup :: DomNode -> Revertible m

      -- | Children virtual nodes
      , children :: Array (VNode m)
      }

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
  | VPrune (Exists (PruneE m))


newtype PruneE m p = PruneE
  { keyPath :: Array String
  , params :: p
  , unsureEq :: p -> p -> Unsure Boolean
      -- ^ Typeclass instance. Manually managed because Purescript doesn't have
      --   native support for existentials (not to mention constrained existentials)
  , render :: p -> VNode m
  }


-- | Change the underlying monad of a virtual node
hoist1 :: forall m n. Functor n => (m ~> n) -> VNode m -> VNode n
hoist1 f = case _ of
  VRawNode x -> VRawNode x
  VRawHtml x -> VRawHtml x
  VText x -> VText x
  VTag { tag, attrs, listeners, fixup, children } ->
    VTag { tag, attrs
         , listeners: listeners # map (map f)
         , fixup: fixup # map (Rev.hoist f)
         , children: children # map (hoist1 f)
         }
  VPrune e -> VPrune $ e # mapExists \(PruneE { keyPath, params, unsureEq, render }) ->
    PruneE { keyPath, params, unsureEq, render: render >>> hoist1 f }

  where _restore = field @"restore"


-- | Prepend a key to pruning paths of all pruned descendants (including self)
addPruneKey :: forall m. String -> VNode m -> VNode m
addPruneKey key = case _ of
  VRawNode x -> VRawNode x
  VRawHtml x -> VRawHtml x
  VText x -> VText x
  VTag { tag, attrs, listeners, fixup, children } ->
    VTag { tag, attrs, fixup, listeners
         , children: children # map (addPruneKey key)
         }
  VPrune e -> VPrune $ e # mapExists \(PruneE { keyPath, params, unsureEq, render }) ->
    PruneE { params, unsureEq
           , keyPath: [key] <> keyPath
           , render: render >>> addPruneKey key
           }

-- | Remove all contained `VPrune` nodes, collapsing their deferred
-- | computation.
-- |
-- | Preserves the container <span style="display: contents"> in order to
-- | minimize difference between a VDom before and after being unpruned
unPrune1 :: forall m. VNode m -> VNode m
unPrune1 = case _ of
  VRawNode x -> VRawNode x
  VRawHtml x -> VRawHtml x
  VText x -> VText x
  VTag { tag, attrs, listeners, fixup, children } ->
    VTag { tag, attrs, listeners, fixup
         , children: children # map unPrune1
         }
  VPrune e ->
    e # runExists \(PruneE { params, render }) ->
          withPruneWrapper [render params]


-- FIXME: We treat VPrune nodes as being a <span style="display: contents"> over
--        their children. This makes diffing easier but is fundamentally a hack.
--        Note that the diffing algorithm directly uses knowledge of this hack.
withPruneWrapper :: forall m. Array (VNode m) -> VNode m
withPruneWrapper children = VTag
  { tag: "span"
  , attrs: Assoc.fromFoldable [ "style" /\ "display: contents" ]
  , listeners: Assoc.fromFoldable []
  , fixup: mempty
  , children
  }


type CaseVNode = forall m r.
     VNode m
  -> (DomNode -> r)
  -> (String -> r)
  -> (String -> r)
  -> ({ tag :: String
      , attrs :: Assoc String String
      , listeners :: Assoc String (DomEvent -> m Unit)
      , fixup :: DomNode -> Revertible m
      , children :: Array (VNode m)
      } -> r)
  -> (Exists (PruneE m) -> r)
  -> r

caseVNode :: CaseVNode
caseVNode node vRawNode vRawHtml vText vTag vPrune =
  case node of
    VRawNode x -> vRawNode x
    VRawHtml x -> vRawHtml x
    VText x -> vText x
    VTag x -> vTag x
    VPrune x -> vPrune x



-- | Virtual DOM type
-- |
-- | This is a `Monoid`, which has two important implications:
-- |
-- | - Every `Html` value is actually a so-called "fragment", meaning
-- |   that it can consist of more than one adjacent elements. For example,
-- |   just as there is an `Html` value representing `<b>text</b>`, there is
-- |   also one representing `<i>text</i><b>text</b>`
-- |
-- | - This type can be used with functions like `foldMap` and monoidal `when`,
-- |   which can be extremely convenient when constructing `Html` values
newtype Html m s = Html (Array (VNode (MationT m s)))

instance FreeMonoid (Html m s) (VNode (MationT m s))

derive instance Newtype (Html m s) _
derive newtype instance Semigroup (Html m s)
derive newtype instance Monoid (Html m s)


-- | `Html` constructor
mkRawNode :: forall m s. DomNode -> Html m s
mkRawNode node = FM.singleton $ VRawNode node

-- | `Html` constructor
mkRawHtml :: forall m s. String -> Html m s
mkRawHtml html = FM.singleton $ VRawHtml html

-- | `Html` constructor
mkText :: forall m s. String -> Html m s
mkText text = FM.singleton $ VText text

-- | `Html` constructor
mkTag :: forall m s.
  { tag :: String
  , attrs :: Assoc String String
  , listeners :: Assoc String (DomEvent -> MationT m s Unit)
  , fixup :: DomNode -> Revertible (MationT m s)
  , children :: Array (Html m s)
  }
  -> Html m s
mkTag info = Html [ VTag info' ]
  where
  info' = info { children = FM.float info.children }

-- | `Html` constructor
mkPrune :: forall p s m. UnsureEq p => String -> (p -> Html m s) -> p -> Html m s
mkPrune key render params =
  FM.singleton $
    addPruneKey key $  -- Append key to key path of this node and all descendants
      VPrune $ mkExists $ PruneE
        { keyPath: []
        , params
        , unsureEq
        , render: render >>> FM.unwrap >>> withPruneWrapper
        }

-- | Remove pruning from all contained VNodes
unPrune :: forall m s. Html m s -> Html m s
unPrune = FM.map unPrune1

-- | Embed one `Html` within another
enroot :: forall m large small. Functor m => Setter' large small -> Html m small -> Html m large
enroot len (Html arr) = Html $ arr # map (hoist1 (MationT.enroot len))

-- | Transform the underlying monad of an `Html`
-- |
-- | The given `m ~> n` is expected to be a monad morphism
hoist :: forall m n a. Functor n => (m ~> n) -> Html m a -> Html n a
hoist f (Html arr) = Html $ arr # map (hoist1 (MationT.hoist f))

