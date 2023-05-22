module Mation.Core.Util.FreeMonoid where

import Prelude as Prelude
import Mation.Core.Prelude hiding (map)

import Data.Array as Array


-- | In a number of places in this codebase, we follow a certain pattern. We have
-- | some datatype which at callsites will typically be consumed as an array, so
-- | we want to make that datatype a monoid.
-- | 
-- | For instance, we have type `Style ≅ String /\ String` which is consumed by
-- | eg `style' :: Array Style -> Prop m s`. It would be useful for `Style` to be
-- | an instance of `Monoid` so that we may write things like
-- |
-- | ```purs
-- | style'
-- |   [ styleOne
-- |   , styleTwo
-- |   , when condition
-- |       someConditionalStyle
-- |   , flip foldMap someFoldable
-- |       \item -> someDynamicStyle
-- |   ]
-- | ```
-- | 
-- | We do this by renaming the type `T` to `T1`, representing "just one" `T`, and then
-- | defining `T ≅ Array T1` with a `Monoid` instance inherited from `Array`.
-- | 
-- | For instance with `Style` we get the redefinition
-- |
-- | ```purs
-- | type Style1 ≅ String /\ String
-- | type Style ≅ Array Style1
-- | instance Monoid Style where ...
-- | ```
-- | 
-- | 
-- | The `FreeMonoid` typeclass (below) is intended to act as a focal point for this pattern.
-- | An instance of the pattern using types `Thing` and `Thing1` should
-- | instantiate `FreeMonoid` as
-- |
-- | ```purs
-- | instance FreeMonoid Thing Thing1
-- | ```
-- | 
-- | The typeclass `FreeMonoid` does not contain much logic. The value is more in providing
-- | a name for the pattern (`FreeMonoid`), a single place to talk about it (this module),
-- | as well a place to put any logic there does happen to be.
-- | 
-- | 
-- | One may wonder why we use a `FreeMonoid` class instead of defining a datatype
-- |
-- | ```purs
-- | newtype FreeMonoid a = FreeMonoid (Array a)
-- | ```
-- | 
-- | and then write e.g.
-- |
-- | ```purs
-- | type Style a = FreeMonoid (Style1 a)
-- | ```
-- | 
-- | The reason for this is that, in this codebase, most such "many types" (eg `Style`)
-- | are exported and exposed to the user. Preferring a datatype over a type alias
-- | means that the user does not have to worry about (or even know!) what
-- | a '`FreeMonoid`' is.
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


-- | Flatten upwards
-- |
-- | Since a `t` is an array of `t1`, then a `Array t` is actually
-- | an `Array (Array t1)`. This turns it into an `Array t1`.
float :: forall t t1. FreeMonoid t t1 => Array t -> Array t1
float arr = arr >>= coerce

-- | Inject
singleton :: forall t t1. FreeMonoid t t1 => t1 -> t
singleton = Array.singleton >>> coerce

wrap :: forall t t1. FreeMonoid t t1 => Array t1 -> t
wrap = coerce

unwrap :: forall t t1. FreeMonoid t t1 => t -> Array t1
unwrap = coerce

map :: forall t t1. FreeMonoid t t1 => (t1 -> t1) -> (t -> t)
map f = unwrap >>> Prelude.map f >>> wrap

