module Mation.Core.Patch where

import Mation.Core.Prelude

import Mation.Core.Util.Assoc as Assoc
import Mation.Core.Mation (Mation)
import Mation.Core.Html (VNode (..), CaseVNode, caseVNode)
import Mation.Core.Dom (DomNode, DomEvent)


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
patchOnto :: forall m s.
  { toEff :: Mation m s -> Effect Unit
  , old :: Maybe (VNode (Mation m s))
  , new :: VNode (Mation m s)
  }
  -> DomNode -> Effect Unit
patchOnto { toEff, old, new } =
  patch_f
    caseMaybe
    casePair
    caseVNode
    { mOldVNode: map (map toEff) old
    , newVNode: map toEff new
    }


foreign import patch_f ::
     CaseMaybe
  -> CasePair
  -> CaseVNode
  -> { mOldVNode :: Maybe (VNode (Effect Unit))
     , newVNode :: VNode (Effect Unit)
     }
  -> DomNode -> Effect Unit



type CaseMaybe =
  forall a r. Maybe a -> r -> (a -> r) -> r

-- | Case analysis on `Maybe`
caseMaybe :: CaseMaybe
caseMaybe maybe nothing just =
  case maybe of
    Nothing -> nothing
    Just a -> just a


type CasePair =
  forall a b r. (a /\ b) -> (a -> b -> r) -> r

casePair :: CasePair
casePair (a /\ b) f = f a b
