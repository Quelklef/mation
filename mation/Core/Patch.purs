module Mation.Core.Patch where

import Mation.Core.Prelude

import Mation.Core.Html (VNode (..), PruneE (..))
import Mation.Core.Dom (DomNode, DomEvent)
import Mation.Core.Util.UnsureEq (Unsure (..))
import Mation.Core.Util.Revertible as Rev
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Exists (Exists, mapExists)


-- | This type is morally the same as `VNode Effect Unit`, but is
-- | expressed more simply to reduce FFI pith
data PatchVNode
  = VPRawNode DomNode
  | VPRawHtml String
  | VPText String
  | VPTag
      { tag :: String
      , attrs :: Assoc String String
      , listeners :: Assoc String (DomEvent -> Effect Unit)
      , fixup :: DomNode -> Effect (Effect Unit)
      , children :: Array PatchVNode
      }
  | VPPrune (Exists PatchPruneE)

newtype PatchPruneE p = PatchPruneE
  { keyPath :: Array String
  , params :: p
  , unsureEq :: p -> p -> Unsure Boolean
  , render :: p -> PatchVNode
  }

vNodeToPatchable :: VNode Effect Unit -> PatchVNode
vNodeToPatchable = case _ of
  VRawNode node -> VPRawNode node
  VRawHtml html -> VPRawHtml html
  VText text -> VPText text
  VTag { tag, attrs, listeners, fixup, children } ->
    VPTag
      { tag, attrs
      , listeners: listeners # (map <<< map) giveUnit
      , children: children # map vNodeToPatchable
      , fixup: fixup # map (giveUnit >>> Rev.collapse)
      }
  VWithK withK -> vNodeToPatchable (giveUnit withK)
  VPrune e -> VPPrune $
    e # mapExists \(PruneE { keyPath, params, unsureEq, render }) ->
          PatchPruneE
            { keyPath, params, unsureEq
            , render: render >>> vNodeToPatchable
            }

  where

  giveUnit :: forall a. (Unit -> a) -> a
  giveUnit = (_ $ unit)


-- | Reference to an object holding runtime information necessary to
-- | correctly handle `VPrune` nodes
foreign import data PruneMapRef :: Type


-- | Patch a VDOM onto a DOM Node
-- |
-- | To tell what needs to be modified, the paching algorithm does not compare
-- | the given VDOM to the DOM. Instead, it compares the given VDOM to
-- | the *old* VDOM, modifying the actual DOM where differences exist.
-- |
-- | This is so that the patching algorithm plays well with third-party
-- | javascript. If, for instance, external javascript adds some "_secretInfo"
-- | attribute to a DOM node, we will not remove it during patching as long
-- | as it isn't also added and removed in the VDOM.
-- |
-- | (There is a discussion to be had about whether the programmer making use of
-- | a "_secretInfo" attribute is a good idea or not. However, that's up to
-- | the programmer, not this library, and it's nicer for this library to play
-- | nicely.)
-- |
-- | The downside to this design is that it means the patching is less robust:
-- | if some DOM node attribute is accidentally deleted by external javascript,
-- | for instance, re-rendering the model will not replace it.
patchOnto ::
  { mOldVNode :: Maybe PatchVNode
  , newVNode :: PatchVNode
  , mPruneMap :: Maybe PruneMapRef
  }
  -> DomNode -> Effect PruneMapRef
patchOnto { mOldVNode, newVNode, mPruneMap } =
  patch_f
    { caseMaybe
    , caseUnsure
    , casePatchVNode
    , mPruneMap
    }
    { mOldVNode
    , newVNode
    }


foreign import patch_f ::
   { caseMaybe :: CaseMaybe
   , caseUnsure :: CaseUnsure
   , casePatchVNode :: CasePatchVNode
   , mPruneMap :: Maybe PruneMapRef
   } ->
   { mOldVNode :: Maybe PatchVNode
   , newVNode :: PatchVNode
   }
  -> (DomNode -> Effect PruneMapRef)


type CasePatchVNode = forall r.
     PatchVNode
  -> (DomNode -> r)
  -> (String -> r)
  -> (String -> r)
  -> ({ tag :: String
      , attrs :: Assoc String String
      , listeners :: Assoc String (DomEvent -> Effect Unit)
      , fixup :: DomNode -> Effect (Effect Unit)
      , children :: Array PatchVNode
      } -> r)
  -> (Exists PatchPruneE -> r)
  -> r

casePatchVNode :: CasePatchVNode
casePatchVNode node vRawNode vRawHtml vText vTag vPrune =
  case node of
    VPRawNode x -> vRawNode x
    VPRawHtml x -> vRawHtml x
    VPText x -> vText x
    VPTag x -> vTag x
    VPPrune x -> vPrune x


type CaseMaybe =
  forall a r. Maybe a -> r -> (a -> r) -> r

-- | Case analysis on `Maybe`
caseMaybe :: CaseMaybe
caseMaybe maybe nothing just =
  case maybe of
    Nothing -> nothing
    Just a -> just a


type CaseUnsure =
  forall a r. Unsure a -> (a -> r) -> r -> r

caseUnsure :: CaseUnsure
caseUnsure uc certainly uncertain =
  case uc of
    Surely x -> certainly x
    Unsure -> uncertain


