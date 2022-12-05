module Mation.Core.Html where

import Mation.Core.Prelude

import Mation.Core.Mation (Mation)
import Mation.Core.Mation as Mation
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Assoc as Assoc
import Mation.Core.Many (class Many, float)


foreign import data DOMNode :: Type
foreign import data DOMEvent :: Type

data Html1 m s

    -- | Embed an existing DOMNode directly into the virtual dom
  = HRawNode DOMNode

    -- | Raw HTML
  | HRawHtml String

    -- | Text node
  | HText String

    -- | Virtual node
  | HTag
      { tag :: String
      , attrs :: Assoc String String
      , listeners :: Assoc String (DOMEvent -> Mation m s)
          -- ^
          -- Knowledge of the listeners is needed here so that @embed@
          -- can be implemented
          --
          -- Otherwise we could put listeners in @fixup@
      , fixup :: DOMNode -> Effect Unit
      , children :: Array (Html1 m s)
      }


-- |
-- Virtual DOM type
--
-- Note that this type instantiates @Monoid@, meaning that it can be used
-- with functions like @when@ and @foldMap@. This can be very handy when
-- constructing @Html@ values!
newtype Html m s = Html (Array (Html1 m s))

instance Many (Html m s) (Html1 m s)

derive instance Newtype (Html m s) _
derive newtype instance Semigroup (Html m s)
derive newtype instance Monoid (Html m s)


mkRawNode :: forall m s. DOMNode -> Html m s
mkRawNode node = Html [ HRawNode node ]

mkRawHtml :: forall m s. String -> Html m s
mkRawHtml html = Html [ HRawHtml html ]

mkText :: forall m s. String -> Html m s
mkText text = Html [ HText text ]

mkTag :: forall m s.
  { tag :: String
  , attrs :: Assoc String String
  , listeners :: Assoc String (DOMEvent -> Mation m s)
  , fixup :: DOMNode -> Effect Unit
  , children :: Array (Html m s)
  }
  -> Html m s
mkTag info = Html [ HTag info' ]
  where
  info' = info { children = float info.children }


-- | Embed one @Html@ within another
-- FIXME: rename? probably -- conflicts with <embed>
embed :: forall m large small. Lens' large small -> Html m small -> Html m large
embed lens (Html arr) = Html $ map (embed1 lens) arr

embed1 :: forall m large small. Lens' large small -> Html1 m small -> Html1 m large
embed1 lens = case _ of
  HRawNode node -> HRawNode node
  HRawHtml html -> HRawHtml html
  HText text -> HText text
  HTag { tag, attrs, listeners, fixup, children } ->
    HTag
      { tag, attrs, fixup
      , listeners: map (map (Mation.embed lens)) listeners
      , children: map (embed1 lens) children
      }






-- | One vnode property
data Prop1 m s

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

    -- Has no effect
  | PNoop


-- |
--
-- Virtual node properties
--
-- Note that this type instantiates @Monoid@, meaning that it can be used
-- with functions like @when@ and @foldMap@. This can be very handy when
-- constructing @Html@ values!
newtype Prop m s = Prop (Array (Prop1 m s))

instance Many (Prop m s) (Prop1 m s)

derive instance Newtype (Prop m s) _
derive newtype instance Semigroup (Prop m s)
derive newtype instance Monoid (Prop m s)

mkPair :: forall m s. String -> String -> Prop m s
mkPair k v = Prop [ PPair k v ]

mkListener :: forall m s. String -> (DOMEvent -> Mation m s) -> Prop m s
mkListener k v = Prop [ PListener k v ]

mkFixup :: forall m s. (DOMNode -> Effect Unit) -> Prop m s
mkFixup f = Prop [ PFixup f ]

mkNoop :: forall m s. Prop m s
mkNoop = Prop [ PNoop ]



-- | Create an element
mkElement :: forall m s. String -> Array (Prop m s) -> Array (Html m s) -> Html m s
mkElement tag props children =
  mkTag { tag, attrs, listeners, fixup, children }

  where

  flat :: Array (Prop1 m s)
  flat = props >>= \(Prop arr) -> arr

  attrs = Assoc.fromFoldable $
    flat # foldMap case _ of
      PPair k v -> [ k /\ v ]
      _ -> []

  listeners = Assoc.fromFoldable $
    flat # foldMap case _ of
      PListener k v -> [ k /\ v ]
      _ -> []

  fixup =
    flat # foldMap case _ of
      PFixup f -> f
      _ -> mempty

