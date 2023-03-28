module Mation.Core.Prop where

import Mation.Core.Prelude

import Mation.Core.Mation (Mation)
import Mation.Core.Mation as Mation
import Mation.Core.Dom (DomNode, DomEvent)
import Mation.Core.Html (Html, mkTag)
import Mation.Core.Util.FreeMonoid (class FreeMonoid)
import Mation.Core.Util.FreeMonoid as FM
import Mation.Core.Util.Assoc as Assoc


-- | A single vnode property
data Prop1 msg

    -- | Some string HTML attribute, like 'id' or 'style'
  = PPair String String

    -- | Event listener
  | PListener String (DomEvent -> msg)

    -- | Fixup function
    -- |
    -- | This is called on the DOM Node after it is mounted. This variant
    -- | should be used with extreme caution as it gives the programmer enough
    -- | power to circumvent framework safeties.
  | PFixup (DomNode -> Effect { restore :: Effect Unit })
      -- ^ FIXME: 'Effect' or 'm' ?
      --
      -- ^ FIXME: this type for PFixup does not support applying state updates
      --   in a fixup prop, which one may want to do. Eg may want to use a fixup
      --   prop to detect when something is no longer hovered by testing if other
      --   elements are hovered (since mouseout is not really reliable)

    -- | Has no effect
  | PNoop

derive instance Functor Prop1



-- | Virtual node properties
-- |
-- | This is the free monoid over `Prop1`
-- |
-- | Since this type instantiates `Monoid`, it can be used with functions like `when` and `foldMap`.
-- | This can be very handy when constructing `Html` values!
newtype Prop m s = Prop (Array (Prop1 (Mation m s)))

instance FreeMonoid (Prop m s) (Prop1 (Mation m s))

derive instance Newtype (Prop m s) _
derive newtype instance Semigroup (Prop m s)
derive newtype instance Monoid (Prop m s)

mkPair :: forall m s. String -> String -> Prop m s
mkPair k v = FM.singleton $ PPair k v

mkListener :: forall m s. String -> (DomEvent -> Mation m s) -> Prop m s
mkListener k v = FM.singleton $ PListener k v

mkFixup :: forall m s. (DomNode -> Effect { restore :: Effect Unit }) -> Prop m s
mkFixup f = FM.singleton $ PFixup f

mkNoop :: forall m s. Prop m s
mkNoop = FM.singleton $ PNoop


enroot :: forall m large small. Setter' large small -> Prop m small -> Prop m large
enroot len (Prop arr) = Prop $ arr # map (map (Mation.enroot len))

-- | Transform the underlying monad of a `Prop`
-- |
-- | The given `m ~> n` is expected to be a monad morphism
hoist :: forall m n a. (m ~> n) -> Prop m a -> Prop n a
hoist f (Prop arr) = Prop $ arr # map (map (Mation.hoist f))


-- FIXME: is there a good abstraction for the repeated play
--        between Functor and FreeMonoid and enroot and hoist in both Prop and Html?


-- | Create an element
mkElement :: forall m s. String -> Array (Prop m s) -> Array (Html m s) -> Html m s
mkElement tag props children =
  mkTag { tag, attrs, listeners, fixup, children }

  where

  flat :: Array (Prop1 (Mation m s))
  flat = FM.float props

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

