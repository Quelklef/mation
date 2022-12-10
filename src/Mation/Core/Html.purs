
-- | Mation virtual dom

module Mation.Core.Html where
  
import Mation.Core.Prelude

import Mation.Core.Mation (Mation)
import Mation.Core.Mation as Mation
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Assoc as Assoc
import Mation.Core.Dom (DomNode, DomEvent)
import Mation.Core.Util.FreeMonoid (class FreeMonoid)
import Mation.Core.Util.FreeMonoid as FM


-- | One virtual node, parameterized by listener result
data VNode msg

    -- | Embed an existing DomNode directly into the virtual dom
  = VRawNode DomNode

    -- | Raw HTML
  | VRawHtml String

    -- | Text node
  | VText String

    -- | Virtual node
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


derive instance Functor VNode


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
  -> r

-- | Case analysis on `VNode`
caseVNode :: CaseVNode
caseVNode vnode vRawNode vRawHtml vText vTag =
  case vnode of
    VRawNode x -> vRawNode x
    VRawHtml x -> vRawHtml x
    VText x -> vText x
    VTag x -> vTag x



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


-- | Embed one `Html` within another
enroot :: forall m large small. Setter' large small -> Html m small -> Html m large
enroot lens (Html arr) = Html $ map (map (Mation.enroot lens)) arr


