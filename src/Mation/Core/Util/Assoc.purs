module Mation.Core.Util.Assoc where

import Mation.Core.Prelude


-- | A representation of 2-tuples
-- |
-- | For convenience interfacing with foreign code, pairs are represented
-- | as two-element arrays. That is, the value `Pair x y` is equivalent to
-- | the Javascript value `[x, y]`.
foreign import data Pair :: Type -> Type -> Type

foreign import mkPair :: forall a b. a -> b -> Pair a b
foreign import usePair :: forall a b r. (a -> b -> r) -> Pair a b -> r

-- | Association array
newtype Assoc k v = Assoc (Array (Pair k v))

instance Functor (Assoc k) where
  map f (Assoc arr) = Assoc $ arr # map (usePair \a b -> mkPair a (f b))


toArray :: forall k v. Assoc k v -> Array (k /\ v)
toArray (Assoc arr) = arr # map (usePair (\k v -> k /\ v))

fromFoldable :: forall f k v. Foldable f => f (k /\ v) -> Assoc k v
fromFoldable = Assoc <<< foldMap (\(k /\ v) -> [mkPair k v])
