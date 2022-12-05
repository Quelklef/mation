module Mation.Assoc where

import Mation.Prelude


newtype Assoc k v = Assoc (Array (k /\ v))

instance Functor (Assoc k) where
  map f (Assoc arr) = Assoc $ map (\(a /\ b) -> a /\ f b) arr


toArray :: forall k v. Assoc k v -> Array (k /\ v)
toArray (Assoc arr) = arr

fromFoldable :: forall f k v. Foldable f => f (k /\ v) -> Assoc k v
fromFoldable = Assoc <<< foldMap (\x -> [x])
