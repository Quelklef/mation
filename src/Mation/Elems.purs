
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
import Mation.Core.Prop (Prop)
import Mation.Core.Prop as Prop
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

-- | Bring-your-own tagname
mkTag :: forall m s. String -> Array (Prop m s) -> Array (Html m s) -> Html m s
mkTag = Prop.mkElement
