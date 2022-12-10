
-- | Mation virtual dom

module Mation.Core.Html where
  
import Mation.Core.Prelude

import Mation.Core.Mation (Mation)
import Mation.Core.Mation as Mation
import Mation.Core.Util.Assoc (Assoc (..))
import Mation.Core.Util.Assoc as Assoc
import Mation.Core.Dom (DomNode, DomEvent)
import Mation.Core.Util.FreeMonoid (class FreeMonoid)
import Mation.Core.Util.FreeMonoid as FM
import Mation.Core.Util.UnsureEq (class UnsureEq, unsureEq, Unsure (..))
import Mation.Core.Util.Exists (Exists, mkExists, runExists, mapExists)


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


    -- | Request for the runtime to entirely skip diffing this node if possible
    -- |
    -- | The type of data contained in `VPrune` is essentially
    -- |
    -- | ```purs
    -- | exists p. UnsureEq p ∧∧ (Maybe String /\ p /\ (p -> VNode msg))
    -- | ```
    -- |
    -- | Where by `∧∧` I mean the existential counterpart to `=>`; that is, the
    -- | type `exists a. C a ∧∧ T a` is a value of type `T a` for some unknown
    -- | type `a` which *is* known to satisfy the constraint `C`
    -- |
    -- | A `VPrune` can be thought of as containing a deferred computation for
    -- | another VNode: if we were going to create a node as `node = f p`, we might
    -- | choose instead to construct a `VPrune` node containing both `f` and `p`.
    -- |
    -- | The idea is that by deferring the computation `node = f p` we might be able
    -- | to skip it entirely. If we are diffing two `VPrune` nodes respectively
    -- | deferring `node = f p` and `node' = f' p'` and we notice that `f = f'` and
    -- | that `p = p'`, then we know `node' = node`. Since `node` is the "old" node
    -- | already in the DOM, then we can skip diffing entirely.
    -- |
    -- | The trouble with this is that functions aren't generally comparable. So while
    -- | we can perhaps test if `p = p'` by checking the value of `p == p'`, there's
    -- | no comparable way to check if `f = f'`. Our solution to this is to ignore it:
    -- | it is the job of whomever is creating the `VPrune` node to ensure that
    -- | whenever it is diffed with another `VPrune` node, they have the same `f`.
    -- |
    -- | This can be a tall order, to so mitigate the issue a little bit `VPrune` nodes
    -- | also hold onto a `Maybe String` value called their "key", which is supposed
    -- | to act as a kind of identity for the node. If two `VPrune` nodes are being
    -- | diffed and have different keys, it is assumed that their respective contained
    -- | functions of type `p -> VNode msg` are different and hence diffing should not
    -- | be skipped.
    -- |
    -- | To summarize, the diffing algorithm for `VPrune` nodes looks roughly like
    -- | the following.
    -- |
    -- | - Let `prune` and `prune'` respectively denote the old and new `VPrune` nodes
    -- |   for the diffing algorithm. Let them respectively defer
    -- |   computations `node = f p` and `node' = f' p'`, and let `key` and `key'`
    -- |   respectively be their keys.
    -- | - If `key` and `key'` are both non-`Nothing` and inequal, or if one is `Nothing`
    -- |   but the other is not, then compute `node` and `node'` and diff them.
    -- | - Otherwise, compute `unsureEq p p'`; if the result is `Certainly true`, then
    -- |   terminate (ie, do not diff). If there is any other result, compute `node`
    -- |   and `node'` and diff them.
  | VPrune (Exists (PruneE msg))


newtype PruneE msg p = PruneE
  { mKey :: Maybe String
  , params :: p
  , unsureEq :: p -> p -> Unsure Boolean
    -- ^ Typeclass instance. Manually managed because Purescript doesn't have
    --   native support for existentials (not to mention constrained existentials)
  , render :: p -> VNode msg
  }
  -- FIXME: without a `Typeable p` instance we might be applying `unsureEq` to two
  --        different types during diffing. This can cause a runtime exception!


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
    VPrune e -> VPrune $ e # mapExists \(PruneE { mKey, params, unsureEq, render }) ->
      PruneE { mKey, params, unsureEq, render: render >>> map f }



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


-- FIXME: write 'hoist'



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


-- FIXME: Maybe instead of trying to track lenses we can track keys?
--        A call to mkPrune could prepend the `mKey` to all children keys, meaning
--        that we get an entire key path. This could improve the stability
--        of VPrune rendering.
mkPrune :: forall p s m. UnsureEq p => Maybe String -> (p -> Html m s) -> p -> Html m s
mkPrune mKey render params =
  FM.singleton $ VPrune $ mkExists $ PruneE
    { mKey
    , params
    , unsureEq
    , render: render >>> wrap
    }

  where

  -- FIXME: We treat VPrune nodes as being a <span style="display: contents"> over
  --        their children. This makes diffing easier but is fundamentally a hack.
  wrap :: forall n t. Html n t -> VNode (Mation n t)
  wrap html = VTag
    { tag: "span"
    , attrs: Assoc [ "style" /\ "display: contents" ]
    , listeners: Assoc []
    , fixup: mempty
    , children: FM.unwrap html
    }



-- | Embed one `Html` within another
-- FIXME: won't accept changing `Lens'` to `Setter'` for some reason?
enroot :: forall m large small. Lens' large small -> Html m small -> Html m large
enroot len (Html arr) = Html $ map (map (Mation.enroot len)) arr
