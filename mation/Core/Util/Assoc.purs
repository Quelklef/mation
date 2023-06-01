module Mation.Core.Util.Assoc
  ( Pair
  , mkPair
  , usePair
  , Assoc
  , toArray
  , fromFoldable
  , lookup
  ) where

import Mation.Core.Prelude

import Data.Array as Array


-- | A two-tuple type which represents pairs (`x`, `y`) as two-element
-- | arrays `[x, y]`. This is convenient for interfacing with foreign code.
foreign import data Pair :: Type -> Type -> Type

-- | `Pair` constructor
foreign import mkPair :: forall a b. a -> b -> Pair a b

-- | `Pair` destructor
foreign import usePair :: forall a b r. (a -> b -> r) -> Pair a b -> r

-- | Association array
-- |
-- | An `Assoc k v` is like a `Map k v` but represented by an array
-- | of (key, value) pairs.
newtype Assoc k v = Assoc (Array (Pair k v))

-- | Transform the values of an `Assoc`
instance Functor (Assoc k) where
  map f (Assoc arr) = Assoc $ arr # map (usePair \a b -> mkPair a (f b))

-- | Combine two `Assoc`s
-- |
-- | This just appends the underlying arrays
instance Semigroup (Assoc k v) where
  append (Assoc a) (Assoc b) = Assoc (a <> b)

instance Monoid (Assoc k v) where
  mempty = Assoc []


toArray :: forall k v. Assoc k v -> Array (k /\ v)
toArray (Assoc arr) = arr # map (usePair (\k v -> k /\ v))

fromFoldable :: forall f k v. Foldable f => f (k /\ v) -> Assoc k v
fromFoldable = Assoc <<< foldMap (\(k /\ v) -> [mkPair k v])

-- | Look up a key
-- |
-- | If it is present more than once, return the leftmost value.
-- | This makes the `Monoid` instance for `Assoc` in some sense "left-biased"
lookup :: forall k v. Eq k => k -> Assoc k v -> Maybe v
lookup k0 (Assoc arr) = arr # Array.findMap (usePair \k v -> if k == k0 then Just v else Nothing)

