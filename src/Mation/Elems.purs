
-- | Functions for creating `Html` values

module Mation.Elems (module X, module Mation.Elems) where

import Mation.Gen.Tags as X
import Mation.Core.Html (Html (..), enroot) as X

import Mation.Core.Prelude
import Effect (Effect)
import Data.Map as Map
import Data.Map (Map)

import Mation.Core.Html as Html
import Mation.Core.Html (Html)
import Mation.Core.Mation (Mation)
import Mation.Core.Dom (DomNode, DomEvent)
import Mation.Core.Util.Assoc (Assoc (..))
import Mation.Core.Util.FreeMonoid as FM


type Html' s = Html Effect s

-- | Create a text node
text :: forall m s. String -> Html m s
text = Html.mkText

-- | Embed raw html
rawHtml :: forall m s. String -> Html m s
rawHtml = Html.mkRawHtml

-- | Embed a DOM node
rawNode :: forall m s. DomNode -> Html m s
rawNode = Html.mkRawNode

-- | Do-it-yourself element creation
mkTag :: forall m s.
  { tag :: String
  , attrs :: Map String String
  , listeners :: Map String (DomEvent -> Mation m s)
  , fixup :: DomNode -> Effect { restore :: Effect Unit }
  , children :: Array (Html m s)
  }
  -> Html m s
mkTag { tag, attrs, listeners, fixup, children } =
  Html.mkTag
    { tag
    , attrs: Assoc $ Map.toUnfoldable attrs
    , listeners: Assoc $ Map.toUnfoldable listeners
    , fixup
    , children
    }
