module Mation.Core.Html where

import Mation.Core.Prelude

import Mation.Core.Mation (Mation)
import Mation.Core.Mation as Mation
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Assoc as Assoc


foreign import data DOMNode :: Type
foreign import data DOMEvent :: Type

-- | Virtual DOM type
data Html m s

    -- | Embed an existing DOMNode directly into the virtual dom
    -- FIXME: rename this? and perhaps HText? they're both "embeds" in a sense ...
  = HEmbed DOMNode

    -- | Text node
  | HText String

    -- | Virtual node
    -- FIXME: rename this
  | HVirtual
      { tag :: String
      , attrs :: Assoc String String
      , listeners :: Assoc String (DOMEvent -> Mation m s)
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
-- FIXME: rename? probably -- conflicts with <embed>
embed :: forall m large small. Lens' large small -> Html m small -> Html m large
embed lens = case _ of
  HEmbed node -> HEmbed node
  HText text -> HText text
  HVirtual { tag, attrs, listeners, fixup, children } ->
    HVirtual { tag, attrs, fixup
             , listeners: map (map (Mation.embed lens)) listeners
             , children: map (embed lens) children
             }






-- | Property of a virtual node
-- FIXME: rename?
data Prop m s

    -- | Some string HTML attribute, like 'id' or 'style'
  = PPair String String

    -- | Event listener
  | PListener String (DOMEvent -> Mation m s)

    -- |
    -- Fixup function
    --
    -- This is called on the DOM Node after it is mounted. This variant
    -- should be used with extreme caution as it gives the programmer enough
    -- power to circumvent framework safeties.
  | PFixup (DOMNode -> Effect Unit)
      -- ^ FIXME: 'Effect' or 'm' ?

    -- Identity property
  | PNoop



-- | Create an element
mkElement :: forall m s. String -> Array (Prop m s) -> Array (Html m s) -> Html m s
mkElement tag props children =
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
