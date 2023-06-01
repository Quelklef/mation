module Mation.Core.Util.FreeMonoid where

import Prelude as Prelude
import Mation.Core.Prelude hiding (map)

import Data.Array as Array


-- | Instances `FreeMonoid t t1` witness that `t` is a newtype
-- | over `Array t1` (which is the canonical free monoid construction)
-- |
-- | We talk about "`t` being the free monoid over `t1`" rather than saying
-- | more directly that "`t` is `Array t1`" in order to emphasize that we don't
-- | plan to do anything special with the array structure (such as sort it)
-- |
-- | Typically instances of `FreeMonoid` follow the `-1` naming convention,
-- | where if the element type is called `Thing` then the free monoid
-- | type is named `Thing1`.
-- |
-- | ***
-- |
-- | In the mation framework, `Html` is a free monoid over a more
-- | fundamental "single HTML (v)node" type. This means that every `Html` value
-- | is actually an "html fragment" which may contain zero or more actual
-- | HTML (v)nodes. This itself can be handy. Additionally, `Html` being
-- | a free monoid means functions like `foldMap` and `guard` can be used when
-- | creating `Html` values, which turns out to be exceedingly nifty.
-- |
-- | Likewise for some other types
-- |
-- | ***
-- |
-- | The reason that `FreeMonoid` is a typeclass rather than a
-- | newtype `newtype FreeMonoidNT a = FreeMonoid (Array a)` is simply so
-- | that instances `FreeMonoid Thing Thing1` get to choose their `Thing`
-- | type instead of being stuck with `FreeMonoidNT Thing`.
class
    -- | Should be compiler-derived
  ( Newtype t (Array t1)
    -- | Should be newtype-derived
  , Semigroup t
    -- | Should be newtype-derived
  , Monoid t
  ) <=
    FreeMonoid t t1
  | t -> t1


-- | Flatten
-- |
-- | Since a `t` is an array of `t1`, then a `Array t` is actually
-- | an `Array (Array t1)`. This turns it into an `Array t1`.
-- |
-- | This is called `float` rather than `flatten` since we
-- | are "losing the inner array" rather than "losing the outer
-- | array"; it is an "upward flattening"
float :: forall t t1. FreeMonoid t t1 => Array t -> Array t1
float arr = arr >>= coerce

singleton :: forall t t1. FreeMonoid t t1 => t1 -> t
singleton = Array.singleton >>> coerce

wrap :: forall t t1. FreeMonoid t t1 => Array t1 -> t
wrap = coerce

unwrap :: forall t t1. FreeMonoid t t1 => t -> Array t1
unwrap = coerce

map :: forall t t1. FreeMonoid t t1 => (t1 -> t1) -> (t -> t)
map f = unwrap >>> Prelude.map f >>> wrap

