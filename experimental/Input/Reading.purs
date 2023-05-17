module Mation.Experimental.Input.Reading where

import Mation.Core.Prelude


-- | Applicative functor providing a context for reading a value out
-- | of an input
-- |
-- | Say you, the user, are rendering an input in your application. This
-- | input is for `Int`, but its model is `String` because it allows the
-- | user to type in the integer as a string. If the user has typed
-- | in a string which is not valid integer syntax, then attempting
-- | to read the value out of the input will fail.
-- |
-- | This `Reading` applicative manages the possible successes and failures
-- | of trying to read an input. On success we produce the input value; on
-- | failure we produce an error *and* a function `model -> model` which perturbs
-- | the input(s) we read out of.
-- |
-- | The intention behind perturbation is that input reading occurs due to user
-- | action, such as a user submitting a form.
data Reading model e a
  = Failure e (Endo (->) model)
  | Success a

derive instance Generic (Reading model e a) _
derive instance Functor (Reading model e)

instance Bifunctor (Reading model) where
  bimap f g = case _ of
    Failure err perturb -> Failure (f err) perturb
    Success a -> Success (g a)

instance Semigroup e => Apply (Reading model e) where
  apply = case _, _ of
    Failure e1 p1, Failure e2 p2 -> Failure (e1 <> e2) (p1 <> p2)
    Failure e p, Success _ -> Failure e p
    Success _, Failure e p -> Failure e p
    Success f, Success x -> Success (f x)

instance Semigroup e => Applicative (Reading model e) where
  pure = Success

fromEither :: forall model e a . Setter' model Boolean -> Either e a -> Reading model e a
fromEither lens = case _ of
  Left err -> Failure err (Endo (lens .~ true))
  Right val -> Success val

fromEitherNoPerturb :: forall model e a. Either e a -> Reading model e a
fromEitherNoPerturb = case _ of
  Left err -> Failure err mempty
  Right val -> Success val

-- | Monadic bind
andThen :: forall model e a. Reading model e a -> (a -> Reading model e a) -> Reading model e a
andThen reading f = case reading of
  Failure e p -> Failure e p
  Success a -> case f a of
    Failure e p -> Failure e p
    Success a' -> Success a'

runReading :: forall model e a. Reading model e a -> Either (e /\ (model -> model)) a
runReading = case _ of
  Failure error (Endo perturb) -> Left (error /\ perturb)
  Success val -> Right val

enroot :: forall large small e a. Setter' large small -> Reading small e a -> Reading large e a
enroot lens = case _ of
  Failure e (Endo p) -> Failure e (Endo (lens p))
  Success a -> Success a

