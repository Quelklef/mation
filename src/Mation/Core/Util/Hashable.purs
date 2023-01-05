module Mation.Core.Util.Hashable where

import Mation.Core.Prelude


-- | Datatypes which can be hashed
-- |
-- | The hashing function is intended to be a fast one-way function
-- | but is not necessarily cryptographically secure.
-- |
-- | If a type `T` implements both `Eq` and `Hashable`, then it should
-- | abide by the following law. If values `x, y :: T` are equal according
-- | to `Eq`, then they must have the same `hash`.
class Hashable a where
  hash :: a -> String

foreign import m3 :: Array String -> String

instance Hashable String where
  hash s = m3 [s]

instance Hashable Int where
  hash = hash <<< show

instance Hashable Number where
  hash = hash <<< show

instance Hashable a => Hashable (Array a) where
  hash = m3 <<< map hash

instance (Hashable a1, Hashable a2) => Hashable (a1 /\ a2) where
  hash (a1 /\ a2) = hash [hash a1, hash a2]
