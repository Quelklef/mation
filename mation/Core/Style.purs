module Mation.Core.Style where

import Mation.Core.Prelude

import Data.Function as Fun
import Data.Map as Map

import Mation.Core.Prop (Prop, mkFixup)
import Mation.Core.Dom (DomNode)
import Mation.Core.Util.FreeMonoid (class FreeMonoid)
import Mation.Core.Util.FreeMonoid as FM
import Mation.Core.Util.Weave (Weave)
import Mation.Core.Util.Weave as W
import Mation.Core.Util.Hashable (class Hashable, hash)
import Mation.Core.Util.IsEndo (class IsEndo, runEndo, toEndo, composeEndoLTR)
import Mation.Core.Util.Revertible as Rev


-- | Represents a single CSS style
-- |
-- | Styles are contextualized by a so-called "scope" which designates
-- | when the style applies.
-- |
-- | This type has one parameter, called `endo`. We expect that `endo`
-- | homomorphs into `Endo (->)`; specifically, we expect
-- | an `IsEndo endo` instance to exist.
-- |
-- | We use different instantiations of `endo` throughout
-- | the codebase. Most of the time we use `endo ~ Weave`, which gives
-- | us `Eq (endo String)` and `Hashable (endo String)` instances.
-- | We also use `endo ~ Endo (->)` when finally turning our styles
-- | into CSS.
newtype Style1 :: (Type -> Type) -> Type
newtype Style1 endo = Style1

    -- | Inline CSS string, e.g. `"line-height: 100em"`
  { css :: String

    -- | Style scopes
  , scopes :: Scopes endo

    -- | Style prelude
    -- |
    -- | Emitted right before the style block in the result CSS
    -- |
    -- | E.g. `"@keyframes my-animation { 0% { color: red; } 100% { color: blue; } }"`
  , prelude :: String
  }

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

-- | Note that this `Eq` instance treats the style CSS as a string, so
-- | if two `Style1` values are the same except that the CSS of one
-- | has a trailing semicolon, then the values will be considered inequal.
instance Eq (endo String) => Eq (Style1 endo) where
  eq = eq `Fun.on` \(Style1 s) -> s

instance Hashable (endo String) => Hashable (Style1 endo) where
  hash = hash <<< \(Style1 s) -> s.css /\ s.scopes.selector /\ s.scopes.block /\ s.prelude

composeScopesLTR :: forall endo. IsEndo (endo String) String => Scopes endo -> Scopes endo -> Scopes endo
composeScopesLTR s s' =
  { selector: composeEndoLTR s.selector s'.selector
  , block: composeEndoLTR s.block s'.block
  }

-- | Change the underling `endo` of a style
-- |
-- | The given function ought to be a monoid morphism in order to preserve
-- | the `Style1` invariant wrt `endo`
changeEndo1 :: forall endo endo'. (endo String -> endo' String) -> Style1 endo -> Style1 endo'
changeEndo1 f (Style1 { css, scopes: { selector, block }, prelude }) = Style1
  { css, scopes: { selector: f selector, block: f block }, prelude }


-- | `Style1` modifier
addScope :: forall endo. IsEndo (endo String) String => Scopes endo -> (Style1 endo -> Style1 endo)
addScope sco (Style1 { css, scopes, prelude }) = Style1 { css, scopes: composeScopesLTR sco scopes, prelude }

-- | `Style1` modifier
addPrelude :: forall endo. String -> (Style1 endo -> Style1 endo)
addPrelude p (Style1 { css, scopes, prelude }) = Style1 { css, scopes, prelude: prelude <> p }


-- | Given a selector (eg `"#my-element"`), render a `Style1` to a CSS string targeting that selector
toCss :: String -> Style1 (Endo (->)) -> String
toCss selec0 (Style1 { css, scopes, prelude }) =
  prelude <> (runEndo scopes.block $ runEndo scopes.selector selec0 <> " { " <> css <> " }")


-- | Combine same-scoped styles
-- |
-- | So if there are two styles which both happen to target exactly `#my-id:hover`, then
-- | they will be combined (and eventually emitted as a single CSS block)
collate :: forall endo.
  IsEndo (endo String) String => Ord (endo String) =>
  Array (Style1 endo) -> Array (Style1 endo)
collate = collateBy
  { key: \(Style1 { scopes }) -> scopes
  , merge: \(Style1 sty1) (Style1 sty2) -> Style1
      { css: sty1.css <> "; " <> sty2.css
      , scopes: sty1.scopes
      , prelude: if sty1.prelude == sty2.prelude
                 then sty1.prelude  -- no need to repeat
                 else sty1.prelude <> sty2.prelude
      }
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


-- | Represents some CSS styles
newtype Style = Style (Array (Style1 Weave))

derive newtype instance Eq Style
derive newtype instance Hashable Style

derive instance Newtype Style _

derive newtype instance Semigroup Style
derive newtype instance Monoid Style
instance FreeMonoid Style (Style1 Weave)


-- | `Style` constructor
-- |
-- | Directly embeds some inline CSS
mkStyle :: String -> Style
mkStyle css = FM.singleton $ Style1
  { css
  , scopes:
      { selector: W.noop
      , block: W.noop
      }
  , prelude: ""
  }

-- | `Style` constructor
mkPair :: String -> String -> Style
mkPair k v = FM.singleton $ Style1
  { css: k <> ": " <> v
  , scopes:
      { selector: W.noop
      , block: W.noop
      }
  , prelude: ""
  }


-- | Turn a bunch of styles into a `Prop`
toProp :: forall m s. Array Style -> Prop m s
toProp = FM.float >>> toProp'
  where

  toProp' :: Array (Style1 Weave) -> Prop m s
  toProp' styles =
    let
      styleHash = hash styles
      className = "mation-style-" <> styleHash
      getCss _ =
        styles
        -- Collate styles
        # collate
        -- Change `Weave` to `Endo (->)`
        # map (changeEndo1 toEndo)
        -- Turn each style into a CSS string
        # map (toCss ("." <> className))
        -- Combine them
        # intercalate "\n"

    in mkFixup \node -> Rev.mkRevertibleE do
      { restore: restoreCss } <- putCss { getCss, hash: styleHash }
      { restore: restoreClass } <- putClass node className
      pure $ liftEffect (restoreCss <> restoreClass)


foreign import putCss :: { getCss :: Unit -> String, hash :: String } -> Effect { restore :: Effect Unit }
foreign import putClass :: DomNode -> String -> Effect { restore :: Effect Unit }

