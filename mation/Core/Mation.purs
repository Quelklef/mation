module Mation.Core.Mation where

import Mation.Core.Prelude

import Effect.Ref as Ref


-- | A `Mation m s` is a computation living in `m` with the ability to execute
-- | pure updates on some state of type `s`. (ie, it can discharge functions
-- | of type `s -> s`)
-- |
-- | ***
-- | 
-- | The most common form of Mation are those which perform a single state update
-- | in a synchronous manner and then terminate. Most event handlers will fall
-- | into this category. For instance, an `onClick` handler may increment a UI model
-- | and then immediately terminate.
-- | 
-- | However, in general Mations can be far more complex. A Mation may be a
-- | long-running process, perhaps performing network requests and incrementally
-- | updating state as the network requests continue, finally terminating when
-- | its job is complete.
-- |
-- | ***
-- | 
-- | It's worth noting that although a Mation is able to update its target state,
-- | it is NOT able to read that state. Usually that means that a Mation only has
-- | knowledge of the state at the time it was created as well as knowledge of any
-- | updates it itself has applied to the state.
-- | 
-- | This restriction is intentional. Chiefely Mations are used as event handlers;
-- | since we want most state to be handled explicitly within the model
-- | it behooves us to be conservative with how much power is given to event
-- | handlers. The choice made by this framework is to allow a Mation to hold its
-- | own state, but only as a limited measure to complete some small task.
-- | 
-- | If it is absolutely necessary for a Mation to be able to listen to state
-- | updates, one can construct it as a closure over a reference to the state.
type Mation m s = Step s -> m Unit

type Mation' s = Mation Effect s

-- | Change the underlying monad of a `Mation`
hoist :: forall m n s. (m ~> n) -> Mation m s -> Mation n s
hoist f = (_ >>> f)

-- | Embed a `Mation`
enroot :: forall m large small. Setter' large small -> Mation m small -> Mation m large
enroot lens = (enrootStep lens >>> _)


-- | A `Step s` is a function that knows how to discharge state updates of
-- | type `s -> s` into `Effect`. In the mation framework, this "discharging"
-- | consists of updating some state reference that lives in `Effect`.
-- |
-- | The coupling to `Effect` is intentional. In general a `Step s` is created
-- | by some computation `d` that knows how to discharge `s -> s` values;
-- | the `Step s` is passed off to some other computation `c` which actually
-- | suplies the `s -> s` values. In principle `d` and `c` could live in any
-- | monads, even different ones, as long as `c`'s monad can interpret `d`'s (since
-- | it has to be able to execute the return value of `Step`).
-- |
-- | The most general form of `Step`, then, would look something like
-- |
-- | ```
-- | type Step m s = forall n. CanExec m n => (s -> s) -> n Unit
-- | ```
-- |
-- | where `CanExec m n` asks for a monad homomorphism `m ~> n`.
-- |
-- | Within the mation framework, though, we know in particular that the
-- | "discharging computation" `d` is the mation runtime, which lives in `Effect`.
-- | Hence, we specialize the type of `Step` to `Effect`.
-- |
-- | We also know that the "child computations" `c` are user event handlers, which
-- | live in some user-chosen monad `n`. Since the event handlers will need to
-- | interpret the results of `Step s`, we will expect that `n`
-- | instantiate `MonadEffect`.
type Step s = forall n. MonadEffect n => (s -> s) -> n Unit

-- | `Step` enroot. Covariant
enrootStep :: forall large small. Setter' large small -> Step large -> Step small
enrootStep lens step endo = step (lens %~ endo)



-- | Transforms a `Step s` stepper into a "buffered stepper" which
-- | stages state updates `s -> s` before applying them all at once.
-- |
-- | Intended to be used with event handlers like
-- |
-- | ```purs
-- | onClick \step -> do
-- |   { stage, apply } <- M.toBuffered step
-- |   ...
-- | ```
-- |
-- | It's tempting to instead write
-- |
-- | ```purs
-- | onClick $ M.toBuffered >=> \{ stage, apply } -> ...`
-- | ```
-- |
-- | but this makes the typesystem choke up due to `Step`
-- | containing a `forall`
toBuffered :: forall m s.
  MonadEffect m => Step s -> m (BufferedStep m s)
toBuffered step = liftEffect do
  buf <- liftEffect $ Ref.new identity
  pure
    { stage: \g -> liftEffect do
        Ref.modify_ (_ >>> g) buf
    , apply: do
        liftEffect (Ref.read buf) >>= step
        liftEffect $ Ref.write identity buf
    }

type BufferedStep m s =
    -- | Add a state update to the buffer
  { stage :: (s -> s) -> m Unit
    -- | Apply all staged updates to the state
  , apply :: m Unit
  }


