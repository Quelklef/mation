module Mation.Core.Util.Hashable where

import Mation.Core.Prelude


-- | Datatypes which can be hashed
-- |
-- | By "hash" we mean a *fast* one-way function, not a *secure*
-- | one-way function.
-- |
-- | If a type `T` implements both `Eq` and `Hashable`, then it should
-- | abide by the following law:
-- |
-- | - if `x == y` then `hash x == hash y`
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

