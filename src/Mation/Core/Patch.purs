module Mation.Core.Patch where

import Mation.Core.Prelude

import Mation.Core.Html (VNode, CaseVNode, caseVNode)
import Mation.Core.Dom (DomNode)
import Mation.Core.Util.UnsureEq (Unsure (..))


-- | Reference to an object holding runtime information necessary to
-- | correctly handle `VPrune` nodes
foreign import data PruneMapRef :: Type


-- | Instead of patching onto a DOM Node directly, we diff the old and new state
-- | to generate a patch function which is then applied to a DOM node.
-- |
-- | This is so that the patching algorithm does not react to changes made to
-- | the DOM by external javascript. If, for instance, external javascript adds
-- | some "_secretInfo" attribute to a DOM node, we will not remove it during
-- | patching.
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
  { mOldVNode :: Maybe (VNode (Effect Unit))
  , newVNode :: VNode (Effect Unit)
  , mPruneMap :: Maybe PruneMapRef
  }
  -> DomNode -> Effect PruneMapRef
patchOnto { mOldVNode, newVNode, mPruneMap } =
  patch_f
    { caseMaybe
    , caseUnsure
    , caseVNode
    , mPruneMap
    }
    { mOldVNode
    , newVNode
    }


foreign import patch_f ::
   { caseMaybe :: CaseMaybe
   , caseUnsure :: CaseUnsure
   , caseVNode :: CaseVNode
   , mPruneMap :: Maybe PruneMapRef
   } ->
   { mOldVNode :: Maybe (VNode (Effect Unit))
   , newVNode :: VNode (Effect Unit)
   }
  -> (DomNode -> Effect PruneMapRef)



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
    Certainly x -> certainly x
    Unsure -> uncertain
