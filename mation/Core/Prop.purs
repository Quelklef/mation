module Mation.Core.Prop where

import Mation.Core.Prelude

import Mation.Core.Dom (DomNode, DomEvent)
import Mation.Core.Html (Html, mkTag)
import Mation.Core.Util.FreeMonoid (class FreeMonoid)
import Mation.Core.Util.FreeMonoid as FM
import Mation.Core.Util.Revertible (Revertible)
import Mation.Core.Util.Revertible as Rev
import Mation.Core.Util.Assoc as Assoc


-- | A single VNode property
data Prop1 m k

    -- | Some string HTML attribute, like 'id' or 'style'
  = PPair String String

    -- | Event listener
  | PListener String (DomEvent -> k -> m Unit)

    -- | Fixup function
    -- |
    -- | This is called on the DOM Node after it is mounted. This variant
    -- | should be used with extreme caution as it gives the programmer enough
    -- | power to circumvent framework safeties.
  | PFixup (DomNode -> k -> Revertible m)

    -- | Has no effect
  | PNoop


instance Contravariant (Prop1 m) where
  cmap f = case _ of
    PPair key val -> PPair key val
    PListener key lis -> PListener key (lis # map (f >>> _))
    PFixup fu -> PFixup (fu # map (f >>> _))
    PNoop -> PNoop


-- | Change the underlying monad of a property
hoist1 :: forall m n k. Functor n => (m ~> n) -> Prop1 m k -> Prop1 n k
hoist1 f = case _ of
  PPair key val -> PPair key val
  PListener key lis -> PListener key (lis # (map <<< map) f)
  PFixup fu -> PFixup (fu # (map <<< map) (Rev.hoist f))
  PNoop -> PNoop


-- | Virtual node properties
-- |
-- | This is the free monoid over `Prop1`
-- |
-- | Since this type instantiates `Monoid`, it can be used with functions like `when` and `foldMap`.
-- | This can be very handy when constructing `Html` values!
newtype Prop m k = Prop (Array (Prop1 m k))

instance FreeMonoid (Prop m k) (Prop1 m k)

derive instance Newtype (Prop m k) _
derive newtype instance Semigroup (Prop m k)
derive newtype instance Monoid (Prop m k)

instance Contravariant (Prop m) where
  cmap f (Prop arr) = Prop (arr # map (cmap f))

-- | `Prop` constructor
mkPair :: forall m k. String -> String -> Prop m k
mkPair key val = FM.singleton $ PPair key val

-- | `Prop` constructor
mkListener :: forall m k. String -> (DomEvent -> k -> m Unit) -> Prop m k
mkListener key val = FM.singleton $ PListener key val

-- | `Prop` constructor
mkFixup :: forall m k. (DomNode -> k -> Revertible m) -> Prop m k
mkFixup f = FM.singleton $ PFixup f

-- | `Prop` constructor
mkNoop :: forall m k. Prop m k
mkNoop = FM.singleton $ PNoop


-- | Transform the underlying monad of a `Prop`
-- |
-- | The given `m ~> n` is expected to be a monad morphism
hoist :: forall m n k. Functor n => (m ~> n) -> Prop m k -> Prop n k
hoist f (Prop arr) = Prop $ arr # map (hoist1 f)

-- | Create an `Html` from an array of `Prop`s
mkTagFromProps :: forall m k. String -> Array (Prop m k) -> Array (Html m k) -> Html m k
mkTagFromProps tag props children =
  mkTag { tag, attrs, listeners, fixup, children }

  where

  flat :: Array (Prop1 m k)
  flat = FM.float props

  attrs = Assoc.fromArray $
    flat # foldMap case _ of
      PPair key val -> [ key /\ val ]
      _ -> []

  listeners = Assoc.fromArray $
    flat # foldMap case _ of
      PListener key val -> [ key /\ val ]
      _ -> []

  fixup =
    flat # foldMap case _ of
      PFixup f -> f
      _ -> mempty

