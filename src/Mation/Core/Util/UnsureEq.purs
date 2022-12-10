module Mation.Core.Util.UnsureEq where

import Mation.Core.Prelude


data Unsure a = Unsure | Certainly a


-- FIXME: have instances try reference equality before anything else


-- | Like `Eq`, but allows a type implement partial equality.
-- |
-- | The canonical example is function types, where if two functions
-- | are identical as references, we know they must be equivalent, but
-- | otherwise we know nothing.
class UnsureEq a where
  unsureEq :: a -> a -> Unsure Boolean

instance UnsureEq Number where unsureEq = viaEq
instance UnsureEq Int where unsureEq = viaEq
instance UnsureEq String where unsureEq = viaEq

instance (UnsureEq a, UnsureEq b) => UnsureEq (a /\ b) where
  unsureEq (a /\ b) (a' /\ b') = unsureEq a a' `and` unsureEq b b'

and :: Unsure Boolean -> Unsure Boolean -> Unsure Boolean
and (Certainly false) _ = Certainly false
and _ (Certainly false) = Certainly false
and Unsure _ = Unsure
and _ Unsure = Unsure
and _ _ = Certainly true



-- | Implementation for `unsureEq` via `eq`
viaEq :: forall a. Eq a => (a -> a -> Unsure Boolean)
viaEq a b = Certainly (a == b)


-- | Implementation for `unsureEq` via reference equality
viaRef :: forall a. (a -> a -> Unsure Boolean)
viaRef a b = case primEq a b of
  true -> Certainly true
  false -> Unsure

foreign import primEq :: forall a b. a -> b -> Boolean
