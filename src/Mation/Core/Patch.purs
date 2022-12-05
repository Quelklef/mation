module Mation.Core.Patch where

import Mation.Core.Prelude

import Mation.Core.Util.Assoc as Assoc
import Mation.Core.Mation (Mation)
import Mation.Core.Html (Html1 (..))
import Mation.Core.Dom (DomNode, DomEvent)


-- | Foreign interface Html type
newtype Html1_f = Html1_f (
    forall r.
       (DomNode -> r)
    -> (String -> r)
    -> (String -> r)
    -> ({ tag :: String
        , attrs :: Array { name :: String, value :: String }
        , listeners :: Array { name :: String, handler :: DomEvent -> Effect Unit }
        , fixup :: DomNode -> Effect Unit
        , children :: Array Html1_f
        } -> r)
    -> r
  )

to_f :: forall m s. (Mation m s -> Effect Unit) -> Html1 m s -> Html1_f
to_f toEff h = Html1_f \hRawNode hRawHtml hText hTag ->
  case h of
    HRawNode node -> hRawNode node
    HRawHtml html -> hRawHtml html
    HText text -> hText text
    HTag { tag, attrs, listeners, fixup, children } ->
      hTag
        { tag
        , attrs: attrs # Assoc.toArray # map \(name /\ value) -> { name, value }
        , listeners: listeners # Assoc.toArray # map \(name /\ handler) -> { name, handler: map toEff handler }
        , fixup
        , children: to_f toEff <$> children
        }


-- |
--
-- Instead of patching onto a DOM Node directly, we diff the old and new state
-- to generate a patch function which is then applied to a DOM node.
--
-- This is so that the patching algorithm does not react to changes made to
-- the DOM by external javascript. If, for instance, external javascript adds
-- some "_secretInfo" attribute to a DOM node, we will not remove it during
-- patching.
--
-- (There is a discussion to be had about whether the programmer making use of
-- a "_secretInfo" attribute is a good idea or not. However, that's up to
-- the programmer, not this library, and it's nicer for this library to play
-- nicely.)
--
-- The downside to this design is that it means the patching is less robust:
-- if some DOM node attribute is accidentally deleted by external javascript,
-- for instance, re-rendering the model will not replace it.
patchOnto :: forall m s.
  { toEff :: Mation m s -> Effect Unit
  , old :: Maybe (Html1 m s)
  , new :: Html1 m s
  }
  -> DomNode -> Effect Unit
patchOnto { toEff, old, new } =
  patch_f
    caseMaybe
    { mOldHtml: to_f toEff <$> old
    , newHtml: to_f toEff new
    }

  where

  caseMaybe :: forall a r. Maybe a -> r -> (a -> r) -> r
  caseMaybe maybe nothing just =
    case maybe of
      Nothing -> nothing
      Just a -> just a

foreign import patch_f ::
     (forall a r. Maybe a -> r -> (a -> r) -> r)
  -> { mOldHtml :: Maybe Html1_f
     , newHtml :: Html1_f
     }
  -> DomNode -> Effect Unit
