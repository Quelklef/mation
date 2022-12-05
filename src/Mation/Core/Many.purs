module Mation.Core.Many where

import Mation.Core.Prelude


{- |

In a number of places in this codebase, we follow a certain pattern. We have
some datatype which at callsites will typically be consumed as an array, so
we want to make that datatype a monoid.

For instance, we have type Style ≅ String /\ String which is consumed by
eg style' :: Array Style -> Prop m s. It would be useful for Style to be an
instance of Monoid so that we may write things like

  style'
    [ styleOne
    , styleTwo
    , when condition
        someConditionalStyle
    , flip foldMap someFoldable
        \item -> someDynamicStyle
    ]

We do this by renaming the type T to T1, representing "just one" T, and then
defining T ≅ Array T1 with a Monoid instance inherited from Array.

For instance with Style we get the redefinition

  type Style1 ≅ String /\ String
  type Style ≅ Array Style1
  instance Monoid Style where ...


The Many typeclass (below) is intended to act as a focal point for this pattern.
An instance of the pattern using types Thing and Thing1 should instantiate Many as

  instance Many Thing Thing1

The typeclass Many does not contain much logic. The value is more in providing
a name for the pattern (Many), a single place to talk about it (this module),
as well a place to put any logic there does happen to be.


One may wonder why we use a Many class instead of defining a datatype

  newtype Many a = Many (Array a)

and then write e.g.

  type Style a = Many (Style1 a)

The reason for this is that, in this codebase, most such "many types" (eg Style)
are exported and exposed to the user. Preferring a datatype over a type alias
means that the user does not have to worry about (or even know!) what a 'Many' is.

-}


class
  ( Newtype t (Array t1)
  , Semigroup t
      -- ^ Should be newtype-derived
  , Monoid t
      -- ^ Should be newtype-derived
  ) <= Many t t1


-- |
--
-- Flatten upwards
--
-- Since a 't' is an array of 't1', then a 'Array t' is actually
-- an 'Array (Array t1)'. This turns it into an 'Array t1'.
float :: forall t t1. Many t t1 => Array t -> Array t1
float arr = arr >>= coerce
