module Mation.Core.Util.Revertible where

import Mation.Core.Prelude

import Data.Array as Array
import Data.Either (either)


-- | A `Revertible m` is an action in the monad `m`
-- | which can be *undone*. Think of `Revertible m` as roughly like
-- | the type `m (m Unit)`, where an action `act :: m (m Unit)` does
-- | something and returns a value `rev :: m Unit` which undoes `act`.
-- |
-- | ***
-- |
-- | `Revertible m` differs from `m (m Unit)` in the following ways:
-- |
-- | - The `Monoid` instance for `Revertible` implements sequencing,
-- |   whereas many `Monoid` instances for `Monad`s implement pointwise-`<>`
-- |
-- | - One can sequence `Revertible m` values without
-- |   an `Applicative m` constraint, and can create a `Revertible m`
-- |   out of an `Effect (Effect Unit)` without a `MonadEffect m` constraint
-- |
-- |   Actually, these constraints aren't entirely obviated, but are
-- |   deferred until the `collapse` destructor.
-- |
-- | This second bullet is somewhat enigmatic but actually very important
-- | for the Mation codebase. By using `Revertible1, the
-- | framework is able to avoid a `MonadEffect` constraint many
-- | API affordances, instead deferring the constraint to the point
-- | where the user actually runs their application.
data Revertible m

  = M (m (m Unit))

      -- Freely embed `Effect (Effect Unit)` values
  | E (Effect (Effect Unit))

      -- Free sequencing
  | Seq (Array (Revertible m))


-- | Create a `Revertible m` from an `m (m Unit)`
mkRevertibleM :: forall m. (m (m Unit)) -> Revertible m
mkRevertibleM = M

-- | Create a `Revertible m` from an `Effect (Effect Unit)`
mkRevertibleE :: forall m. (Effect (Effect Unit)) -> Revertible m
mkRevertibleE = E

-- | Semigroup under action sequencing
instance Semigroup (Revertible m) where
  append (Seq r1s) (Seq r2s) = Seq (r1s <> r2s)
  append (Seq r1s) r2 = Seq (r1s <> [r2])
  append r1 (Seq r2s) = Seq ([r1] <> r2s)
  append r1 r2 = Seq [r1, r2]

-- | Monoid under action sequencing
instance Monoid (Revertible m) where
  mempty = Seq []

-- I don't think we can get rid of this Functor constraint =(
--
-- The enroot function for Html and Prop both inhreit this constraint,
-- which is somewhat saddening
hoist :: forall m n. Functor n => (m ~> n) -> Revertible m -> Revertible n
hoist f = case _ of
  M m -> M (m # f # map f)
  E e -> E e
  Seq rs -> Seq (rs # map (hoist f))

collapse :: forall m. MonadEffect m => Revertible m -> (m (m Unit))
collapse = case _ of
  M m -> m
  E e -> e # liftEffect # map liftEffect
  Seq ms -> ms # traverse collapse # map (foldl (*>) (pure unit))

