module Mation.Core.Util.Weave
  ( Weave
  , this
  , that
  , noop
  , empty
  , module ReExport
  ) where

import Mation.Core.Util.IsEndo (runEndo, (.>>), (.<<), (.<>)) as ReExport

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
-- | Such a function `f` is represented (approximately) as the array
-- |
-- | ```
-- | [ Just a1, Nothing, Just a2, Nothing, ... ]
-- | ```
-- |
-- | Where a `Just` gives a known value of type `a` and a `Nothing`
-- | value represents the function parameter.
newtype Weave a = Weave (Array (Elem a))

-- | `That` represents the parameter of the represented function,
-- | and `This a` represents some known vaue of type `a`
data Elem a = That | This a

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
    That -> hash $ 1
    This x -> hash $ 2 /\ x

instance Monoid a => IsEndo (Weave a) a where

  -- | Interprets each `Hole` as the function parameter and
  -- | interpret element juxtaposition as monoidal concatenation.
  runEndo :: Weave a -> (a -> a)
  runEndo (Weave elems) =
    elems # foldMap case _ of
      That -> identity
      This a -> const a

  -- | Compose two `Weave`s left-to-right as functions
  -- |
  -- | Semantics: `runEndo (composeLTR f g) = runEndo f >>> runEndo g`
  composeEndoLTR :: Weave a -> Weave a -> Weave a
  composeEndoLTR (Weave xs) (Weave ys) = Weave $
    ys >>= case _ of
      That -> xs
      This y -> [ This y ]

  -- | Concatenate two `Weave`s
  -- |
  -- | Semantics: `runEndo (concat f g) = runEndo f <> runEndo g`
  concatEndo :: Weave a -> Weave a -> Weave a
  concatEndo (Weave xs) (Weave ys) = Weave (xs <> ys)


-- | Semigroup under pointwise concatenation (`concatEndo`)
instance Monoid a => Semigroup (Weave a) where
  append = concatEndo

-- | Monoid under pointwise concatenation
instance Monoid a => Monoid (Weave a) where
  mempty = Weave []


-- | Represents a known value
this :: forall a. a -> Weave a
this a = Weave [ This a ]

-- | Represents the function parameter
that :: forall a. Weave a
that = Weave [ That ]

-- | Represents the identity function
-- |
-- | Semantics: `runEndo noop = identity`
-- |
-- | Equivalent to `that`
noop :: forall a. Weave a
noop = Weave [ That ]

-- | Represents `const mempty`
-- |
-- | Semantics: `runEndo empty = const mempty`
-- |
-- | Equivalent to `this mempty`
empty :: forall a. Monoid a => Weave a
empty = Weave []

