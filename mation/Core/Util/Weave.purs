module Mation.Core.Util.Weave (module Mation.Core.Util.Weave, module X) where

import Mation.Core.Util.IsEndo ((.>>), (.<<), (.<>)) as X

import Mation.Core.Prelude hiding (compose)

import Mation.Core.Util.Hashable (class Hashable, hash)
import Mation.Core.Util.IsEndo (class IsEndo, concatEndo)


-- | Given a type `A` instantiating `Monoid`, the type `Weave A` is a
-- | concrete representation of functions `f :: A -> A` of the form
-- |
-- | ```
-- | f x = a1 <> x <> a2 <> x <> a3 <> x <> ...
-- | ```
-- |
-- | for some `a1`, `a2`, `a3`, ...
-- |
-- | Such a function `f` is represented specifically as the array
-- |
-- | ```
-- | [ Just a1, Nothing, Just a2, Nothing, ... ]
-- | ```
-- |
-- | Where a `Just` gives a known value of type `a` and a `Nothing`
-- | value represents the function parameter.
-- |
-- | ***
-- |
-- | Actually, we don't use `Maybe` but an isomorphic type
-- | called `Elem`, and we allow repeated `x` terms and adjacent `a_n`
-- | terms. But it's all equivalent.
newtype Weave a = Weave (Array (Elem a))

-- | `Hole` represents the parameter of the represented function,
-- | and `Elem a` represents a fixed value `a :: a`
data Elem a = Hole | Elem a

derive instance Generic (Weave a) _
derive instance Eq a => Eq (Weave a)
  -- ^ Technically this instance is invalid because it doesn't normalize,
  --   so for instance it considers `Weave []` distinct from `Weave [mempty]`.
  --   This isn't a problem for our use-sites though.
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

-- | Represents a function parameter (aka `Weave` "hole")
that :: forall a. Weave a
that = Weave [ Hole ]

