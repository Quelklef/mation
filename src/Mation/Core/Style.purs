module Mation.Core.Style where

import Mation.Core.Prelude

import Data.String.Common (joinWith)
import Data.Function as Fun
import Data.Map as Map

import Mation.Core.Prop (Prop, mkFixup)
import Mation.Core.Dom (DomNode)
import Mation.Core.Util.FreeMonoid (class FreeMonoid)
import Mation.Core.Util.FreeMonoid as FM
import Mation.Core.Util.Weave (Weave)
import Mation.Core.Util.Weave as W
import Mation.Core.Util.Hashable (class Hashable, hash)


-- | Represents a single CSS style
-- |
-- | Styles are contextualized by a so-called "scope" which designates
-- | when the style applies.
-- |
-- | This type has one parameter, called `endo`. We expect that `endo`
-- | monoid-embeds into `Endo (->)` polymorphically; that is, there exists
-- | some function `f :: endo ~> Endo (->)` which is a monoid-embedding
-- | for every choice of type parameter instantiation.
newtype Style1 :: (Type -> Type) -> Type
newtype Style1 endo = Style1
    -- | Inline CSS string, e.g. `"line-height: 100em"`
  { css :: String
    -- | Style scopes
  , scopes :: Scopes endo
  }

-- | Note that this `Eq` instance treats the style CSS as a string, so
-- | if two `Style1` values are the same except that the CSS of one
-- | has a trailing semicolon, then the values will be considered inequal.
instance Eq (endo String) => Eq (Style1 endo) where
  eq = eq `Fun.on` \(Style1 s) -> s

instance Hashable (endo String) => Hashable (Style1 endo) where
  hash = hash <<< \(Style1 s) -> s.css /\ s.scopes.selector /\ s.scopes.block


--- | Represents block- and selector-level scopings for CSS styles.
--- |
--- | For instance,
--- |
--- | ```
--- |  { selector: \sel -> sel <> " > *:nth-child(2)"
--- |  , block: \css -> "@media (max-width: 500px) { @print { " <> css <> " }}"
--- |  }
--- | ```
type Scopes endo =
  -- | Scope a style with some selector combinator, eg `_ <> ":focus"`
  { selector :: endo String
  -- | Scope a style by some block combinator, eg `\css -> "@media print { " <> css <> " }"`
  , block :: endo String
  }


-- | Change the underling `endo` of a style
-- |
-- | The given function ought to be a monoid morphism in order to preserve
-- | the `Style1` invariant wrt `endo`
hoistStyle1 :: forall endo endo'. (endo String -> endo' String) -> Style1 endo -> Style1 endo'
hoistStyle1 f (Style1 { css, scopes: { selector, block } }) = Style1
  { css, scopes: { selector: f selector, block: f block } }


-- | Given a selector (eg `"#my-element"`), render a `Style1`
-- | styles to a CSS string targeting that selector
toCss :: String -> Style1 (Endo (->)) -> String
toCss selec0 (Style1 { css, scopes }) =
    runEndo scopes.block $ runEndo scopes.selector selec0 <> " { " <> css <> " }"

  where
  runEndo :: forall c a. Endo c a -> c a a
  runEndo (Endo f) = f


-- | Combine the CSS strings for styles that share a scope
collate :: forall endo. Ord (endo String) => Array (Style1 endo) -> Array (Style1 endo)
collate = collateBy
  { key: \(Style1 { scopes }) -> scopes
  , merge: \(Style1 sty1) (Style1 sty2) -> Style1 { css: sty1.css <> "; " <> sty2.css, scopes: sty1.scopes }
  }

  where

  collateBy :: forall k a. Ord k =>
    { key :: a -> k
    , merge :: a -> a -> a
    } -> Array a -> Array a
  collateBy { key, merge } =
    map (\a -> key a /\ a)
    >>> Map.fromFoldableWith merge
    >>> Map.toUnfoldableUnordered
    >>> map (\(_ /\ v) -> v)


-- | Represents a bunch of styles
newtype Style = Style (Array (Style1 Weave))

derive newtype instance Eq Style
derive newtype instance Hashable Style

derive instance Newtype Style _

derive newtype instance Semigroup Style
derive newtype instance Monoid Style
instance FreeMonoid Style (Style1 Weave)

-- | `Style` constructor
mkPair :: String -> String -> Style
mkPair k v = FM.singleton $ Style1
  { css: k <> ": " <> v
  , scopes:
      { selector: W.noop
      , block: W.noop
      }
  }

-- | `Prop` constructor
toProp :: forall m s. Array Style -> Prop m s
toProp = FM.float >>> toProp'
  where

  toProp' :: forall m s. Array (Style1 Weave) -> Prop m s
  toProp' styles =
    let
      styleHash = hash styles
      className = "mation-style-" <> styleHash
      getCss _ =
        styles
        # collate
        # map (hoistStyle1 (Endo <<< W.runWeave))
        # map (toCss ("." <> className))
        # intercalate "\n"

    in mkFixup \node -> do
      restoreCss <- putCss { getCss, hash: styleHash }
      restoreClass <- putClass node className
      pure (restoreCss <> restoreClass)

      -- TODO: Pretty sure the `fixup` API has a leak problem right now. If a node
      --       is removed from the DOM due to its parent being removed (ie, the node
      --       itself does not get diffed) then the fixup lifecycle will not
      --       complete. WRT styles this manifests as 'zombie' <style> tags within
      --       the stylsheet hangout; ie, <style>s which are no longer needed but
      --       are still present.

foreign import putCss :: { getCss :: Unit -> String, hash :: String } -> Effect { restore :: Effect Unit }
foreign import putClass :: DomNode -> String -> Effect { restore :: Effect Unit }

