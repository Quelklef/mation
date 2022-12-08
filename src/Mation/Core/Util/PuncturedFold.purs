module Mation.Core.Util.PuncturedFold where

import Mation.Core.Prelude

import Data.Hashable (class Hashable, hash)


-- | Given a type `A` instantiating `Monoid`, we are occasionally
-- | interested in those functions of type `A -> A` which are a chain
-- | of applications of `<>` awaiting some single argument, eg
-- |
-- | ```
-- | f = \x -> a1 <> x <> a2 <> a3 <> x
-- | f = \x -> fold [ a1, x, a2, a3, x ]
-- | ```
-- |
-- | The type `PuncturedFold A` gives a concrete representation to
-- | exactly those functions. That is, the type `PuncturedFold A` is
-- | a subscture of `A -> A` (relative to both composition and
-- | concatenation; see the functions below) for which we have nice
-- | properties like the ability to write an `Eq` instance.
-- |
-- | We think of these functions as calls to `fold` awaiting a single
-- | value to complete. That value has been "punched out" of the call,
-- | hence the name `PuncturedFold`.
-- |
-- |
-- | A `PuncturedFold A` is represented as an array of elemens, each of
-- | which is either an element of `A` or a `Hole`, which represents
-- | the awaited argument. For instance, the above function `f` would
-- | be expressed as
-- |
-- | ```
-- | pf = [ Elem a1, Hole, Elem a2, Elem a3, Hole ]
-- | ```
newtype PuncturedFold a = PF (Array (Elem a))

data Elem a = Hole | Elem a


derive instance Generic (PuncturedFold a) _
derive instance Eq a => Eq (PuncturedFold a)
derive newtype instance Hashable a => Hashable (PuncturedFold a)

derive instance Generic (Elem a) _
derive instance Eq a => Eq (Elem a)

instance Hashable a => Hashable (Elem a) where
  hash = case _ of
    Hole -> hash $ 1
    Elem x -> hash $ 2 /\ x


toEndo :: forall a. Monoid a => PuncturedFold a -> Endo' a
toEndo = toEndo_ >>> Endo

toEndo_ :: forall a. Monoid a => PuncturedFold a -> (a -> a)
toEndo_ (PF elems) =
  elems # foldMap case _ of
    Hole -> identity
    Elem a -> const a


-- | Concatenate two punctured folds
-- |
-- | This is the preimage of `<>` under `toEndo_`.
-- | That is, ``toEndo_ (pf1 `concat` pf2) = toEndo_ pf1 <> toEndo_ pf2``
concat :: forall a. PuncturedFold a -> PuncturedFold a -> PuncturedFold a
concat (PF xs) (PF ys) = PF (xs <> ys)


-- | Compose two punctured folds
-- |
-- | This is the preimage of `<>` under `toEndo`.
-- | That is, ``toEndo (pf1 `compose` pf2) = toEndo pf1 <> toEndo pf2``
compose :: forall a. PuncturedFold a -> PuncturedFold a -> PuncturedFold a
compose (PF xs) (PF ys) = PF $
  ys >>= case _ of
    Hole -> xs
    Elem y -> [ Elem y ]
