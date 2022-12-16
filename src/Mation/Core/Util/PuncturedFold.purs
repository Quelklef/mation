module Mation.Core.Util.PuncturedFold where

import Mation.Core.Prelude

import Mation.Core.Util.Hashable (class Hashable, hash)


-- | Given a type `A` instantiating `Monoid`, we are occasionally
-- | interested in those functions of type `A -> A` which are a chain
-- | of applications of `<>` awaiting some single argument, eg
-- |
-- | ```
-- | f = \x -> a1 <> x <> a2 <> a3 <> x
-- |   = \x -> fold [ a1, x, a2, a3, x ]
-- | ```
-- |
-- | We think of these functions as calls to `fold`, but where
-- | one value has been "punced out" of the `fold`.
-- |
-- | The type `PuncturedFold A` gives a concrete representation to the
-- | collection of such functions, considered as a substructure
-- | of `A -> A`. This concrete representation exhibits desirable features
-- | like existince of a well-formed `Eq` instance.
-- |
-- | A `PuncturedFold A` is represented as an array of elemens, each of
-- | which is either an element of `A` or a `Hole`, which represents
-- | the awaited argument. For instance, the above function `f` would
-- | be expressed as
-- |
-- | ```
-- | pf = PF [ Elem a1, Hole, Elem a2, Elem a3, Hole ]
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


-- | Turn a `PuncturedFold` into an `Endo (->)`.
-- |
-- | This interprets each `Hole` a parameter and interprets
-- | element juxtaposition as monoidal concatenation.
-- |
-- | For instance,
-- |
-- | ```
-- | toEndoCom (PF [ Elem "anti-", Hole, Elem "ism" ])
-- | = Endo (\str -> "anti-" <> str <> "ism")
-- | ```
toEndoCom :: forall a. Monoid a => PuncturedFold a -> Endo' a
toEndoCom = toEndoCat >>> Endo

-- | Same as `toEndo` but without the `Endo` newtype
-- |
-- | Note that `a -> a` and `Endo (->) a` have different monoid
-- | instances. The functions `toEndoCom` and `toEndoCat`, which
-- | differ only by `newtype`, are different monoid-injections
-- | from `PuncturedFold a` into `a -> a`.
-- |
-- | Specifically, `toEndoCom` takes `<>` to function composition,
-- | and `toEndoCat` takes `<>` to function-result-concatenation.
-- |
-- | For instance, if we let
-- |
-- | ```
-- | brace = PF [ Elem "{", Hole, "}" ]
-- | brack = PF [ Elem "[", Hole, "]" ]
-- | ```
-- |
-- | Then we get
-- |
-- | ```
-- | (let Endo f = toEndoCom brace <> toEndoCom brack in f "😄")
-- | == "{[😄]}"
-- | (let Endo f = toEndoCat brace <> toEndoCat brack in f "😄")
-- | == "{😄}[😄]"
-- | ```
toEndoCat :: forall a. Monoid a => PuncturedFold a -> (a -> a)
toEndoCat (PF elems) =
  elems # foldMap case _ of
    Hole -> identity
    Elem a -> const a


-- | Concatenate two punctured folds
-- |
-- | This is the preimage of `<>` under `toEndoCat`.
-- | That is, ``toEndoCat (pf1 `concat` pf2) = toEndoCat pf1 <> toEndoCat pf2``
concat :: forall a. PuncturedFold a -> PuncturedFold a -> PuncturedFold a
concat (PF xs) (PF ys) = PF (xs <> ys)


-- | Compose two punctured folds
-- |
-- | This is the preimage of `<>` under `toEndoCom`.
-- | That is, ``toEndoCom (pf1 `compose` pf2) = toEndoCom pf1 <> toEndoCom pf2``
compose :: forall a. PuncturedFold a -> PuncturedFold a -> PuncturedFold a
compose (PF xs) (PF ys) = PF $
  ys >>= case _ of
    Hole -> xs
    Elem y -> [ Elem y ]
