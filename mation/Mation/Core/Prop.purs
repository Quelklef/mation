module Mation.Core.Prop where

import Mation.Core.Prelude

import Mation.Core.Mation (Mation)
import Mation.Core.MationT (Step, MationT (..))
import Mation.Core.MationT as MationT
import Mation.Core.Dom (DomNode, DomEvent)
import Mation.Core.Html (Html, mkTag)
import Mation.Core.Util.FreeMonoid (class FreeMonoid)
import Mation.Core.Util.FreeMonoid as FM
import Mation.Core.Util.Revertible (Revertible)
import Mation.Core.Util.Revertible as Rev
import Mation.Core.Util.Assoc as Assoc


-- | A single vnode property
data Prop1 m

    -- | Some string HTML attribute, like 'id' or 'style'
  = PPair String String

    -- | Event listener
  | PListener String (DomEvent -> m Unit)

    -- | Fixup function
    -- |
    -- | This is called on the DOM Node after it is mounted. This variant
    -- | should be used with extreme caution as it gives the programmer enough
    -- | power to circumvent framework safeties.
  | PFixup (DomNode -> Revertible m)

    -- | Has no effect
  | PNoop


hoist1 :: forall m n. Functor n => (m ~> n) -> Prop1 m -> Prop1 n
hoist1 f = case _ of
  PPair k v -> PPair k v
  PListener k lis -> PListener k (f <$> lis)
  PFixup fu -> PFixup (fu # map (Rev.hoist f))
  PNoop -> PNoop

  where
  _restore = prop (Proxy :: Proxy "restore")



-- | Virtual node properties
-- |
-- | This is the free monoid over `Prop1`
-- |
-- | Since this type instantiates `Monoid`, it can be used with functions like `when` and `foldMap`.
-- | This can be very handy when constructing `Html` values!
newtype Prop m s = Prop (Array (Prop1 (MationT m s)))

instance FreeMonoid (Prop m s) (Prop1 (MationT m s))

derive instance Newtype (Prop m s) _
derive newtype instance Semigroup (Prop m s)
derive newtype instance Monoid (Prop m s)

-- | `Prop` constructor
mkPair :: forall m s. String -> String -> Prop m s
mkPair k v = FM.singleton $ PPair k v

-- | `Prop` constructor
mkListener :: forall m s. String -> (DomEvent -> Mation m s) -> Prop m s
mkListener k v = FM.singleton $ PListener k (MationT <<< v)

-- | `Prop` constructor
mkFixup :: forall m s. (DomNode -> Revertible (MationT m s)) -> Prop m s
mkFixup f = FM.singleton $ PFixup f

-- | `Prop` constructor
mkNoop :: forall m s. Prop m s
mkNoop = FM.singleton $ PNoop


enroot :: forall m large small. Functor m => Setter' large small -> Prop m small -> Prop m large
enroot lens (Prop arr) = Prop $ arr # map (hoist1 (MationT.enroot lens))

-- | Transform the underlying monad of a `Prop`
-- |
-- | The given `m ~> n` is expected to be a monad morphism
hoist :: forall m n a. Functor n => (m ~> n) -> Prop m a -> Prop n a
hoist f (Prop arr) = Prop $ arr # map (hoist1 (MationT.hoist f))


-- FIXME: is there a good abstraction for the repeated play
--        between Functor and FreeMonoid and enroot and hoist in both Prop and Html?


-- | Create an `Html` from an array of `Prop`s
mkTagFromProps :: forall m s. String -> Array (Prop m s) -> Array (Html m s) -> Html m s
mkTagFromProps tag props children =
  mkTag { tag, attrs, listeners, fixup, children }

  where

  flat :: Array (Prop1 (MationT m s))
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

