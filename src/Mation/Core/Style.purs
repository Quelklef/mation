module Mation.Core.Style where

import Mation.Core.Prelude

import Data.Hashable (class Hashable, hash)
import Data.Array as Array

import Mation.Core.Html (Prop, mkPair, mkFixup)
import Mation.Core.Dom (DomNode)
import Mation.Core.Many (class Many, float)
import Mation.Core.Util.PuncturedFold (PuncturedFold)
import Mation.Core.Util.PuncturedFold as PF


-- | Represents a bunch of styles
-- |
-- | This type has one parameter named `endo`. The expectation is
-- | that `endo` be some substructure of `Endo`. In particular, we expect
-- | that there be some function `f` so
-- | that `(endo String, f) ↪ (Endo String, <>)` as a monid. I don't think
-- | we use this fact outright, but it's good for illustration.
-- |
-- | Despite the name `Style1`, this type does represent a *bunch* of styles.
-- | The reason for the `-1` suffix is that we still form a monoid closure
-- | over it with `Style` since turning `Style1` into a lawful monoid
-- | directly would not be easy.
data Style1 :: (Type -> Type) -> Type
data Style1 endo

    -- | K-v pair, eg `SPair "position" "fixed"`
  = SPair String String

    -- | Scope a style with some selector combinator, eg
    -- |
    -- | ```
    -- | SScopeASelector (_ <> ":focus") (SPair "color" "red")
    -- | ```
    -- |
    -- | Note that using `const` as the selector *can* be used to
    -- | generate global styles. Not recommended, though.
  | SScopeASelector (endo String) (Style1 endo)

    -- | Scope a style by some block combinator, eg
    -- |
    -- | ```
    -- | SScopeABlock "@media (max-width: 500px)" (SPair "color" "blue")
    -- | ```
  | SScopeABlock String (Style1 endo)

    -- | Concatenation
    -- |
    -- | This is a constructor over an `Array` instead of a binary constructor
    -- | for no reason other than it happens to be convenient elsewhere
  | SConcat (Array (Style1 endo))


instance Eq (endo String) => Eq (Style1 endo) where
  eq (SPair k v) (SPair k' v') = k == k' && v == v'
  eq (SScopeASelector sco sty) (SScopeASelector sco' sty') = sco == sco' && sty == sty'
  eq (SScopeABlock sco sty) (SScopeABlock sco' sty') = sco == sco' && sty == sty'
  eq (SConcat xs) (SConcat ys) = xs == ys
  eq _ _ = false

instance Hashable (endo String) => Hashable (Style1 endo) where
  hash = case _ of
    SPair a b -> hash $ 1 /\ a /\ b
    SScopeASelector s a -> hash $ 2 /\ s /\ a
    SScopeABlock s a -> hash $ 3 /\ s /\ a
    SConcat xs -> hash $ 4 /\ xs

mapStyle1 :: forall endo endo'. (endo String -> endo' String) -> (Style1 endo -> Style1 endo')
mapStyle1 f = case _ of
  SPair k v -> SPair k v
  SScopeASelector sco sty -> SScopeASelector (f sco) (mapStyle1 f sty)
  SScopeABlock sco sty -> SScopeABlock sco (mapStyle1 f sty)
  SConcat xs -> SConcat (map (mapStyle1 f) xs)


-- | Given a target selector (eg `"#my-element"`), render a `Style1` to CSS targeting that selector
toCss :: String -> Style1 (Endo (->)) -> String
toCss selec0 = linearize >>> emit

  where

  -- Not particularly efficient, but it'll do for now

  linearize ::
       Style1 (Endo (->))
    -> Array
         { css :: String
         , selectorScope :: Endo' String
         , blockScope :: Endo' String
         }
  linearize = case _ of
    SPair k v ->
        [ { css: k <> ": " <> v <> ";"
          , selectorScope: mempty
          , blockScope: mempty
          }
        ]
    SScopeASelector endo style ->
      linearize style # map (_selectorScope %~ (endo <> _))
    SScopeABlock scope style ->
      let endo = Endo \block -> scope <> " { " <> block <> " } "
      in linearize style # map (_blockScope %~ (endo <> _))
    SConcat xs -> xs >>= linearize

  emit :: Array { css :: String, selectorScope :: Endo' String, blockScope :: Endo' String } -> String
  emit = foldMap emit1

  emit1 { css, selectorScope, blockScope } =
      runEndo blockScope $ runEndo selectorScope selec0 <> " { " <> css <> " } "

  _blockScope = prop (Proxy :: Proxy "blockScope")
  _selectorScope = prop (Proxy :: Proxy "selectorScope")



toProp1 :: forall m s. Style1 PuncturedFold -> Prop m s
toProp1 style =
  let
    className = "_mationcss-" <> show (hash style)
    selector = "." <> className
    css = toCss selector (mapStyle1 PF.toEndoCom style)
  in mkFixup \node -> do
    restoreCss <- putCss { css, forClass: className }
    restoreClass <- putClass node className
    pure $ { restore: restoreCss *> restoreClass }

foreign import putCss :: { css :: String, forClass :: String } -> Effect (Effect Unit)
foreign import putClass :: DomNode -> String -> Effect (Effect Unit)



runEndo :: forall c a. Endo c a -> c a a
runEndo (Endo f) = f



newtype Style = Style (Array (Style1 PuncturedFold))

instance Many Style (Style1 PuncturedFold)

derive instance Newtype Style _
derive newtype instance Semigroup Style
derive newtype instance Monoid Style


toProp :: forall m s. Array Style -> Prop m s
toProp = float >>> foldMap toProp1
