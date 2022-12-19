module Mation.Core.Style where

import Mation.Core.Prelude

import Data.String.Common (joinWith)

import Mation.Core.Prop (Prop, mkFixup)
import Mation.Core.Dom (DomNode)
import Mation.Core.Util.FreeMonoid (class FreeMonoid)
import Mation.Core.Util.FreeMonoid as FM
import Mation.Core.Util.PuncturedFold (PuncturedFold)
import Mation.Core.Util.PuncturedFold as PF
import Mation.Core.Util.Hashable (class Hashable, hash)


-- | Represents a bunch of styles
-- |
-- | This type has one parameter named `endo`. The expectation is
-- | that `endo` be some substructure of `Endo`. In particular, we expect
-- | that there be some function `f` so that `(endo String, f)` embeds
-- | into `(Endo String, <>)` as a monoid. I don't think
-- | we use this fact outright, but it's good for illustration.
-- |
-- | Despite the name `Style1`, this type represents a *collection* of CSS styles.
-- | The reason for the `-1` suffix is that we still form a monoid closure
-- | over it with `Style` since turning `Style1` into a lawful monoid
-- | directly would not be easy.
data Style1 :: (Type -> Type) -> Type
data Style1 endo

    -- | Raw inline style, eg `SInline "position: fixed; top: 0"`
  = SInline String

    -- | Modify the scope of the style
  | SScoped (StyleScope endo) (Style1 endo)

    -- | Concatenation
    -- |
    -- | This is a constructor over an `Array` instead of a binary constructor
    -- | for no reason other than it happens to be convenient elsewhere
  | SConcat (Array (Style1 endo))


-- | Represents both block- and selector-level scopings for CSS styles.
-- |
-- | For instance, the value
-- |
-- | ```
-- | StyleScope { selector: (_ <> " > *:nth-child(2)")
-- |            , block: [ "@media (max-width: 500px)", "@print" ]
-- |            }
-- | ```
-- |
-- | Is to be interpreted as the function accepting a value `THE_SELECTOR`
-- | and a value `THE_STYLES` and producing the CSS
-- |
-- | ```
-- | @print { @media (max-width: 500px) { THE_SELECTOR > *:nth-child(2) { THE_STYLES } } }
-- | ```
newtype StyleScope endo = StyleScope

    -- | Scope a style with some selector combinator, eg `_ <> ":focus"`
    -- |
    -- | Note that using `const` as the selector *can* be used to
    -- | generate global styles. Not recommended, though.
  { selector :: endo String

    -- | Scope a style by some block combinator, eg `\css -> "@media print { " <> css <> " }"`
  , block :: endo String
  }

derive newtype instance Semigroup (endo String) => Semigroup (StyleScope endo)
derive newtype instance Monoid (endo String) => Monoid (StyleScope endo)
derive newtype instance Eq (endo String) => Eq (StyleScope endo)

instance Hashable (endo String) => Hashable (StyleScope endo) where
  hash (StyleScope { selector, block }) = hash $ selector /\ block

mkSelectorScope :: forall endo. Monoid (endo String) => endo String -> StyleScope endo
mkSelectorScope scope = StyleScope { selector: scope, block: mempty }

mkBlockScope :: forall endo. Monoid (endo String) => endo String -> StyleScope endo
mkBlockScope scope = StyleScope { selector: mempty, block: scope }


derive instance Eq (endo String) => Eq (Style1 endo)

instance Hashable (endo String) => Hashable (Style1 endo) where
  hash = case _ of
    SInline s -> hash $ 1 /\ s
    SScoped s a -> hash $ 2 /\ s /\ a
    SConcat xs -> hash $ 3 /\ xs

mapStyle1 :: forall endo endo'. (endo String -> endo' String) -> (Style1 endo -> Style1 endo')
mapStyle1 f = case _ of
  SInline s -> SInline s
  SScoped (StyleScope { selector, block }) sty ->
    SScoped (StyleScope { block: f block, selector: f selector }) (mapStyle1 f sty)
  SConcat xs -> SConcat (map (mapStyle1 f) xs)


-- | Given a target selector (eg `"#my-element"`), render a `Style1` to CSS targeting that selector
toCss :: String -> Style1 (Endo (->)) -> String
toCss selec0 = collate >>> linearize >>> emit

  where

  -- | Combine unscoped styles
  collate :: forall endo. Style1 endo -> Style1 endo
  collate = case _ of
    SInline s -> SInline s
    SConcat styles ->
      let inlines /\ others = styles # partitionWith case _ of
            SInline s -> Left s
            other -> Right other
      in
        SConcat $ [ SInline $ joinWith "; " inlines ] <> others

    other -> other

  -- | Turn a `Style` into an array of inline styles (eg, `color: red; flex: 1`) contextualized by scope
  linearize :: Style1 (Endo (->)) -> Array { css :: String, scope :: StyleScope (Endo (->)) }
  linearize = case _ of
    SInline s -> [ { css: s, scope: mempty } ]
    SScoped sco style -> linearize style # map (_scope %~ (sco <> _))
    SConcat xs -> xs >>= linearize


  -- | Return the result of `linearize` into a CSS string (eg, to be placed into a <style> tag)
  emit :: Array { css :: String, scope :: StyleScope (Endo (->)) } -> String
  emit = foldMap emit1

  emit1 :: { css :: String, scope :: StyleScope (Endo (->)) } -> String
  emit1 { css, scope: StyleScope scope } =
      (runEndo scope.block $ runEndo scope.selector selec0 <> " { " <> css <> " } ") <> "\n"

  _scope = prop (Proxy :: Proxy "scope")

  partitionWith :: forall a x y. (a -> Either x y) -> Array a -> Array x /\ Array y
  partitionWith f = foldMap $ f >>> case _ of Left a -> [a] /\ []
                                              Right a -> [] /\ [a]



toProp1 :: forall m s. Style1 PuncturedFold -> Prop m s
toProp1 style =
  let
    styleHash = hash style
    className = "mation-style-" <> styleHash
    getCss _ = toCss ("." <> className) (mapStyle1 PF.toEndoCom style)
  in mkFixup \node -> do
    restoreCss <- putCss { getCss, hash: styleHash }
    restoreClass <- putClass node className
    pure (restoreCss <> restoreClass)

foreign import putCss :: { getCss :: Unit -> String, hash :: String } -> Effect { restore :: Effect Unit }
foreign import putClass :: DomNode -> String -> Effect { restore :: Effect Unit }

toProp :: forall m s. Array Style -> Prop m s
toProp = FM.float >>> SConcat >>> toProp1

runEndo :: forall c a. Endo c a -> c a a
runEndo (Endo f) = f



-- | Represents some style to place on an `Html` node
newtype Style = Style (Array (Style1 PuncturedFold))

instance FreeMonoid Style (Style1 PuncturedFold)

derive instance Newtype Style _
derive newtype instance Semigroup Style
derive newtype instance Monoid Style

mkPair :: String -> String -> Style
mkPair k v = FM.singleton $ SInline (k <> ": " <> v)


mkScoped :: StyleScope PuncturedFold -> Style -> Style
mkScoped scope (Style styles) = FM.singleton $ SScoped scope (SConcat styles)

