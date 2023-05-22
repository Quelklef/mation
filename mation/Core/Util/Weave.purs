module Mation.Core.Util.Weave (module Mation.Core.Util.Weave, module X) where

import Mation.Core.Util.IsEndo ((.>>), (.<<), (.<>)) as X

import Mation.Core.Prelude hiding (compose)

import Mation.Core.Util.Hashable (class Hashable, hash)
import Mation.Core.Util.IsEndo (class IsEndo, concatEndo)


-- | Given a type `A` instantiating `Monoid`, this gives a concrete
-- | representation to those functions of type `A -> A` which are a chain
-- | of applications of `<>` awaiting some single argument, eg
-- |
-- | ```
-- | f = \x -> a1 <> x <> a2 <> a3 <> x
-- |   = \x -> fold [ a1, x, a2, a3, x ]
-- | ```
-- |
-- | This concrete representation exhibits desirable features
-- | like existince of a well-formed `Eq` instance.
-- |
-- | A `Weave A` is represented as an array of elements, each of
-- | which is either an element of `A` or a `Hole`, which represents
-- | the awaited argument. For instance, the above function `f`
-- | would be expressed as
-- |
-- | ```
-- | w = Weave [ Elem a1, Hole, Elem a2, Elem a3, Hole ]
-- | ```
newtype Weave a = Weave (Array (Elem a))

data Elem a = Hole | Elem a

derive instance Generic (Weave a) _
derive instance Eq a => Eq (Weave a)
instance Ord a => Ord (Weave a) where compare = genericCompare
instance Show a => Show (Weave a) where show = genericShow
derive newtype instance Hashable a => Hashable (Weave a)

derive instance Generic (Elem a) _
derive instance Eq a => Eq (Elem a)
instance Ord a => Ord (Elem a) where compare = genericCompare
instance Show a => Show (Elem a) where show = genericShow

instance Hashable a => Hashable (Elem a) where
  hash = case _ of
    Hole -> hash $ 1
    Elem x -> hash $ 2 /\ x

instance Monoid a => IsEndo (Weave a) a where

  -- | Interprets each `Hole` as the function parameter and
  -- | interpret element juxtaposition as monoidal concatenation.
  runEndo :: Weave a -> (a -> a)
  runEndo (Weave elems) =
    elems # foldMap case _ of
      Hole -> identity
      Elem a -> const a

  -- | Compose two `Weave`s left-to-right as functions
  -- |
  -- | Semantics: `runWeave (composeLTR f g) = runWeave f >>> runWeave g`
  composeEndoLTR :: Weave a -> Weave a -> Weave a
  composeEndoLTR (Weave xs) (Weave ys) = Weave $
    ys >>= case _ of
      Hole -> xs
      Elem y -> [ Elem y ]

  -- | Concatenate two `Weave`s
  -- |
  -- | Semantics: `runWeave (concat f g) = runWeave f <> runWeave g`
  concatEndo :: Weave a -> Weave a -> Weave a
  concatEndo (Weave xs) (Weave ys) = Weave (xs <> ys)


-- | Semigroup under pointwise concatenation
instance Monoid a => Semigroup (Weave a) where
  append = concatEndo

-- | Monoid under pointwise concatenation
instance Monoid a => Monoid (Weave a) where
  mempty = empty


-- | Identity function
-- |
-- | Semantics: `runWeave noop = identity`
noop :: forall a. Weave a
noop = Weave [ Hole ]

-- | Semantics: `runWeave empty = const mempty`
empty :: forall a. Monoid a => Weave a
empty = Weave [ Elem mempty ]

-- | Represent a known value as a `Weave`
this :: forall a. a -> Weave a
this a = Weave [ Elem a ]

-- | Represents a function parameter / `Weave` "hole"
that :: forall a. Weave a
that = Weave [ Hole ]

