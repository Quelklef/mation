module Mation.Html where

import Mation.Prelude

import Mation.Mation (Mation)
import Mation.Mation as Mation
import Mation.Assoc (Assoc)
import Mation.Assoc as Assoc


foreign import data DOMNode :: Type

-- | Virtual DOM type
data Html m s

    -- | Embed an existing DOMNode directly into the virtual dom
    -- FIXME: rename this?
  = HEmbed DOMNode

    -- | Text node
  | HText String

    -- | Virtual node
    -- FIXME: rename this
  | HVirtual
      { tag :: String
      , attrs :: Assoc String String
      , listeners :: Assoc String (Mation m s)
          -- ^
          -- Knowledge of the listeners is needed here so that @embed@
          -- can be implemented
          --
          -- Otherwise we could put listeners in @fixup@
      , fixup :: DOMNode -> Effect Unit
      , children :: Array (Html m s)
      }

-- | Simple Html type
type Html' s = Html Effect s

-- | Embed one @Html@ within another
embed :: forall m large small. Lens' large small -> Html m small -> Html m large
embed lens = case _ of
  HEmbed node -> HEmbed node
  HText text -> HText text
  HVirtual { tag, attrs, listeners, fixup, children } ->
    HVirtual { tag, attrs, fixup
             , listeners: map (Mation.embed lens) listeners
             , children: map (embed lens) children
             }

-- | Foreign interface Html type
newtype Html_f = Html_f (
    forall r.
       (DOMNode -> r)
    -> (String -> r)
    -> ({ tag :: String
        , attrs :: Array { name :: String, value :: String }
        , listeners :: Array { name :: String, handler :: Effect Unit }
        , fixup :: DOMNode -> Effect Unit
        , children :: Array Html_f
        } -> r)
    -> r
  )

to_f :: forall m s. (Mation m s -> Effect Unit) -> Html m s -> Html_f
to_f toEff html = Html_f \hembed htext hvirtual ->
  case html of
    HEmbed node -> hembed node
    HText text -> htext text
    HVirtual { tag, attrs, listeners, fixup, children } ->
      hvirtual
        { tag
        , attrs: attrs # Assoc.toArray # map \(name /\ value) -> { name, value }
        , listeners: listeners # Assoc.toArray # map \(name /\ mation) -> { name, handler: toEff mation }
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
  , old :: Maybe (Html m s)
  , new :: Html m s
  }
  -> DOMNode -> Effect Unit
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
  -> { mOldHtml :: Maybe Html_f
     , newHtml :: Html_f
     }
  -> DOMNode -> Effect Unit



txt :: forall m s. String -> Html m s
txt = HText

elt :: forall m s. String -> Array (Prop m s) -> Array (Html m s) -> Html m s
elt tag props children =
  HVirtual { tag, attrs, listeners, fixup, children }

  where

  attrs = Assoc.fromFoldable $
    props # foldMap case _ of
      PPair k v -> [ k /\ v ]
      _ -> []

  listeners = Assoc.fromFoldable $
    props # foldMap case _ of
      PListener k v -> [ k /\ v ]
      _ -> []

  fixup =
    props # foldMap case _ of
      PFixup f -> f
      _ -> mempty


-- | Property of a virtual node
data Prop m s

    -- | Some string HTML attribute, like 'id' or 'style'
  = PPair String String

    -- | Event listener
  | PListener String (Mation m s)

    -- |
    -- Fixup function
    --
    -- This is called on the DOM Node after it is mounted. This variant
    -- should be used with extreme caution as it gives the programmer enough
    -- power to circumvent framework safeties.
  | PFixup (DOMNode -> Effect Unit)
      -- ^ FIXME: 'Effect' or 'm' ?

att :: forall m s. String -> String -> Prop m s
att = PPair

lis :: forall m s. String -> Mation m s -> Prop m s
lis = PListener







