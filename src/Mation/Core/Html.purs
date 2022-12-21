
-- | Mation virtual dom

module Mation.Core.Html where
  
import Mation.Core.Prelude

import Mation.Core.Mation (Mation)
import Mation.Core.Mation as Mation
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Assoc as Assoc
import Mation.Core.Dom (DomNode, DomEvent)
import Mation.Core.Util.FreeMonoid (class FreeMonoid)
import Mation.Core.Util.FreeMonoid as FM
import Mation.Core.Util.UnsureEq (class UnsureEq, unsureEq, Unsure)
import Mation.Core.Util.Exists (Exists, mkExists, mapExists)


-- | A single Virtual Node. Type is parameterized by listener result.
data VNode msg

    -- | Embed an existing DomNode directly into the virtual dom
  = VRawNode DomNode

    -- | Raw HTML
  | VRawHtml String

    -- | Text node
  | VText String

    -- | Tag node
  | VTag
      { tag :: String
      , attrs :: Assoc String String
      , listeners :: Assoc String (DomEvent -> msg)
          -- ^
          -- Knowledge of the listeners is needed here so that `enroot`
          -- can be implemented
          --
          -- Otherwise we could put listeners in `fixup`
      , fixup :: DomNode -> Effect { restore :: Effect Unit }
      , children :: Array (VNode msg)
      }

    -- | The user can "prune" a virtual node by supplying, instead of an actual
    -- | virtual node, a value of type `param` and a function that can turn it
    -- | into a virtual node. In other words, a deferred computation for a virtual
    -- | node. The user also must supply an `UnsureEq` instance for `param` as well
    -- | as a so-called "key" for the pruned node.
    -- |
    -- | By considering the keys of all pruned nodes in an entire virtual dom, we
    -- | may assign pruned nodes with a so-called "key path", which is the sequence
    -- | of pruned keys from the VDOM root down to the given pruned node.
    -- |
    -- | The user is required to ensure that, over the lifetime of a Mation
    -- | application, whenever two virtual nodes have the same key paths, then
    -- | they also have the same `param` type and computation function.
    -- |
    -- | The punchline to all this is as follows. When we perform VDOM diffing,
    -- | we build up a mapping from pruning keypaths to parameter values. The
    -- | next time we diff and we reach a pruned node, we check that mapping,
    -- | using the prune key path to fetch the parameter value fromt the previous
    -- | frame. If the parameter values match, then--since the computation
    -- | function is not allowed to change--we know that the deferred comptuations
    -- | are exactly equal, and we skip diffing entirely by just re-using the
    -- | DOM node from last frame.
  | VPrune (Exists (PruneE msg))


newtype PruneE msg p = PruneE
  { keyPath :: Array String
  , params :: p
  , unsureEq :: p -> p -> Unsure Boolean
    -- ^ Typeclass instance. Manually managed because Purescript doesn't have
    --   native support for existentials (not to mention constrained existentials)
  , render :: p -> VNode msg
  }


instance Functor VNode where
  map f = case _ of
    VRawNode x -> VRawNode x
    VRawHtml x -> VRawHtml x
    VText x -> VText x
    VTag { tag, attrs, listeners, fixup, children } ->
      VTag { tag, attrs, fixup
           , listeners: listeners # map (map f)
           , children: children # map (map f)
           }
    VPrune e -> VPrune $ e # mapExists \(PruneE { keyPath, params, unsureEq, render }) ->
      PruneE { keyPath, params, unsureEq, render: render >>> map f }


pruneScope :: forall msg. String -> VNode msg -> VNode msg
pruneScope key = case _ of
  VRawNode x -> VRawNode x
  VRawHtml x -> VRawHtml x
  VText x -> VText x
  VTag { tag, attrs, listeners, fixup, children } ->
    VTag { tag, attrs, fixup, listeners
         , children: children # map (pruneScope key)
         }
  VPrune e -> VPrune $ e # mapExists \(PruneE { keyPath, params, unsureEq, render }) ->
    PruneE { params, unsureEq
           , keyPath: [key] <> keyPath
           , render: render >>> pruneScope key
           }


type CaseVNode = forall msg r.
     VNode msg
  -> (DomNode -> r)
  -> (String -> r)
  -> (String -> r)
  -> ({ tag :: String
      , attrs :: Assoc String String
      , listeners :: Assoc String (DomEvent -> msg)
      , fixup :: DomNode -> Effect { restore :: Effect Unit }
      , children :: Array (VNode msg)
      } -> r)
  -> (Exists (PruneE msg) -> r)
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
-- |   which can be extremeley convenient when constructing `Html` values
newtype Html m s = Html (Array (VNode (Mation m s)))

instance FreeMonoid (Html m s) (VNode (Mation m s))

derive instance Newtype (Html m s) _
derive newtype instance Semigroup (Html m s)
derive newtype instance Monoid (Html m s)


mkRawNode :: forall m s. DomNode -> Html m s
mkRawNode node = FM.singleton $ VRawNode node

mkRawHtml :: forall m s. String -> Html m s
mkRawHtml html = FM.singleton $ VRawHtml html

mkText :: forall m s. String -> Html m s
mkText text = FM.singleton $ VText text

mkTag :: forall m s.
  { tag :: String
  , attrs :: Assoc String String
  , listeners :: Assoc String (DomEvent -> Mation m s)
  , fixup :: DomNode -> Effect { restore :: Effect Unit }
  , children :: Array (Html m s)
  }
  -> Html m s
mkTag info = Html [ VTag info' ]
  where
  info' = info { children = FM.float info.children }


mkPrune :: forall p s m. UnsureEq p => String -> (p -> Html m s) -> p -> Html m s
mkPrune key render params =
  FM.singleton $ VPrune $ mkExists $ PruneE
    { keyPath: [key]
    , params
    , unsureEq
    , render: render >>> wrap >>> pruneScope key
    }

  where

  -- FIXME: We treat VPrune nodes as being a <span style="display: contents"> over
  --        their children. This makes diffing easier but is fundamentally a hack.
  --        Note that the diffing algorithm directly uses knowledge of this hack.
  wrap :: forall n t. Html n t -> VNode (Mation n t)
  wrap html = VTag
    { tag: "span"
    , attrs: Assoc.fromFoldable [ "style" /\ "display: contents" ]
    , listeners: Assoc.fromFoldable []
    , fixup: mempty
    , children: FM.unwrap html
    }



-- | Embed one `Html` within another
enroot :: forall m large small. Setter' large small -> Html m small -> Html m large
enroot len (Html arr) = Html $ map (map (Mation.enroot len)) arr

hoist :: forall m n a. (forall b. m b -> n b) -> Html m a -> Html n a
hoist f (Html arr) = Html $ arr # map (map (Mation.hoist f))
