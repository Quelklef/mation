module Mation.Core.Mation where

import Mation.Core.Prelude

import Effect.Ref as Ref


-- | A `Mation m s` is a computation within `m` with the ability to update some
-- | state value of type `s` as it pleases.
-- | 
-- | 
-- | The most common form of mation are those which perform a single state update
-- | in a synchronous manner and then terminate. Most event handlers will fall
-- | into this category. For instance, an `onClick` handler may increment a UI model
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
type Mation m s = Step s -> m Unit


{-

FIXME. For what it's worth, Mation and Daemon are quite similar:

  Daemon m a ≈ (ReadWriteRef a -> m Unit)
  Mation m a ≈ (   UpdateRef a -> m Unit)

for sufficient types ReadWriteRef, UpdateRef.

Might be worthwhile -- or at the very least fun -- to create a module
with the different Ref kinds and then turn Daemon and Mation into
simple type aliases.

-}


-- | Applies a single state update
-- |
-- | ***
-- |
-- | Note the coupling with `MonadEffect` here.
-- |
-- | A `Mation` value is a correspondence between agents that may live in different
-- | monadic contexts. One agent has access to the state and knows how to apply
-- | state updates of type `s -> s`. This agent produces a `Step s`. The other agent
-- | then consumes this `Step s`, invoking it as it pleases.
-- |
-- | We would be wrongly coupling these two agents to assume that they perform their
-- | computations within the same monad. Why should they? Perhaps the stepping agent
-- | only needs `State` to perform a state update but the consuming agent needs
-- | to do some I/O in order to produce state updates for the stepping agent to
-- | consume, and hence the consuming agent lives in `Effect`.
-- |
-- | However, it would also be wrong to *completely* decouple the monads of the two
-- | agents, asserting that they can be any two monads with no restrictions. This
-- | is wrong because the consuming agent needs to be able to actually perform
-- | the `Step s` provided by the stepping agent. That is, when the stepping agent
-- | produces an `(s -> s) -> M Unit` (for some `M`), the consuming agent is going
-- | to be calling it, meaning it needs to know how to execute an `M` in whatever
-- | monad it's working in.
-- |
-- | Hence, the most correct definition for `Mation` and `Step` would look something
-- | like this:
-- |
-- | ```
-- | class CanExec sm cm where
-- |   exec :: forall a. sm a -> cm a
-- |
-- | -- `s` is state type; `sm` is stepping agent monad; `cm` is consuming
-- | -- agent monad
-- | type Mation sm cm s = Step sm s -> cm Unit
-- | type Step sm s = (s -> s) -> sm Unit
-- | ```
-- |
-- | However, juggling two monad parameters sounds annoying. So, we use a simplified
-- | version of this instead.
-- |
-- | We know that the stepping agent will be the Mation framework runtime, and that
-- | that runtime runs in `Effect`. So we fix `sm = Effect` and can swap `CanExec sm`
-- | out with `MonadEffect`.
-- |
-- | ***
-- |
-- | To recap. There are at least three options for how to deal with the monads
-- | relevant to a mation computation. (1) is to conflate the stepping monad `sm`
-- | with the consuming monad `cm` (ie, assume `sm = cm`). (2) is to decouple them
-- | the "right" way using two type parameters and `CanExec`. (3) is like (2) but
-- | we set specifically `sm = Effect` and `CanExec sm = MonadEffect`.
-- |
-- | Concretely, two effects of choosing (3) (or (2)) over (1) are as follows:
-- |
-- | - Sometimes creating a `Mation m a` will require a `MonadEffect` on the `m`
-- |   (resp. would require a `CanExec sm`). (See `Mation` constructors elsewhere in
-- |   this module.) This can be somewhat annoying.
-- |
-- | - We can write a `hoist` function for mations with type
-- |
-- |   ```
-- |   hoist :: forall m n a. (forall b. m b -> n b) -> Mation m a -> Mation n a
-- |   ```
-- |
-- |   This hoist function only requires a transformation `m ~> n`. If we had
-- |   conflated `sm = cm` as in option (1), then writing this hoist function
-- |   would require something stronger, perhaps even a monad *isomorphism*, which
-- |   would make it practically useless.
-- |
-- |   Having this hoist function is important!
--
-- FIXME: Maybe the `CanExec` solution ought to be used after all? Just for the
--        sake of full generality for the `Mation` type?
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


