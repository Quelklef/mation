module Mation.Core.Mation where

import Mation.Core.Prelude

import Effect.Ref as Ref


-- | A `Mation m s` is a computation within `m` with the ability to update some
-- | state value of type `s` as it pleases.
-- | 
-- | 
-- | The most common form of mation are those which perform a single state update
-- | in a synchronous manner and then terminate. Most event handlers will fall
-- | into this category. For instance, an onClick handler may increment a UI model
-- | and then terminate.
-- | 
-- | However, mations in general can be far more complex. A mation may be a
-- | long-running process, perhaps performing network requests and incrementally
-- | updating state as the network requests continue, finally terminating when
-- | its job is complete.
-- | 
-- | 
-- | It's worth noting that although a mation is able to update its target state,
-- | it is NOT able to read that state. Usually that means that a mation only has
-- | knowledge of the state at the time it was created as well as knowledge of any
-- | updates it itself has applied to the state.
-- | 
-- | This restriction is intentional. Chiefely mations are used as event handlers
-- | in HTML; since we want most state to be handled explicitly within the model
-- | it behooves us to be conservative with how much power is given to event
-- | handlers. The choice made by this framework is to allow a mation to hold its
-- | own state, but only as a limited measure to complete some single task.
-- | 
-- | If it is absolutely necessary for a mation to be able to listen to state
-- | updates, one can construct it as a closure over a reference to the state.
-- | 
-- | (nb. I'm sorry to say I don't have any concrete examples or a pointed
-- | explanation of why it would indeed be bad for an event handler to be able to
-- | read the model state. All I have is an intuition, and I'm going with it!)

newtype Mation m s = Mation (Step s -> m Unit)

-- | Applies a single state update
type Step s = forall n. MonadEffect n => (s -> s) -> n Unit


-- | The most powerful form of mation. Callsites to this function
-- | are supplied a function `apply :: Step s -> m Unit`. The apply
-- | function accepts some state update `s -> s` and applies it
-- | to the state. Callsites to `mkCont` may supply `apply` with as
-- | many such state updates as they like, at whatever time they like.
mkCont :: forall m s. (Step s -> m Unit) -> Mation m s
mkCont = Mation

-- | Convenience wrapper around `mkCont` which creates a mation able
-- | to buffer its state updates before applying them. The application
-- | will not re-render until the buffered updates are applied.
-- |
-- | Not only does buffering an update not cause a re-render, it doesn't
-- | even cause a model update. All buffered updates are totally invisible
-- | to the application until they are applied.
mkStaged :: forall m s.
  MonadEffect m =>
     -- | Add a state update to the buffer
  ({ stage :: (s -> s) -> m Unit
     -- | Apply all staged updates to the model
   , apply :: m Unit
   } -> m Unit
  ) -> Mation m s
mkStaged f =
  mkCont \step -> do
    buf <- liftEffect do
      Ref.new (identity :: s -> s)
    f
      { stage: \g -> liftEffect do
          Ref.modify_ (_ >>> g) buf
      , apply: liftEffect do
          Ref.read buf >>= step
          Ref.write identity buf
      }

-- | Apply some known state update
mkPure :: forall m s. MonadEffect m => (s -> s) -> Mation m s
mkPure endo = Mation \step -> liftEffect (step endo)

-- | Do nothing
mkNoop :: forall m s. Applicative m => Mation m s
mkNoop = Mation \_step -> pure unit

-- | Perform an effect and apply the resultant state update
mkEff :: forall m s. MonadEffect m => m (s -> s) -> Mation m s
mkEff getEndo = Mation \step -> getEndo >>= \endo -> liftEffect (step endo)


runMation :: forall m s. Mation m s -> ((s -> s) -> Effect Unit) -> m Unit
runMation (Mation f) step = f (step >>> liftEffect)


-- | Given a witness `Setter'` to `small` being contained within `large`, we
-- | are able to lift a mation on a `small` type to one on a `large` type.
-- |
-- | This is what allows composition of `Html` components!
enroot :: forall m large small. Setter' large small -> Mation m small -> Mation m large
enroot lens (Mation f) =
  Mation \apply -> f \endo -> apply (lens %~ endo)

instance Apply m => Semigroup (Mation m s) where
  append (Mation f) (Mation g) = Mation \step -> f step *> g step

instance Applicative m => Monoid (Mation m s) where
  mempty = Mation \_step -> pure unit

