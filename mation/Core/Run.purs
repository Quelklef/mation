module Mation.Core.Run where

import Mation.Core.Prelude

import Effect.Exception (throw)

import Mation.Core.MationT (MationT)
import Mation.Core.MationT as MationT
import Mation.Core.Daemon (Daemon)
import Mation.Core.Daemon as Daemon
import Mation.Core.Html (Html (..), VNode)
import Mation.Core.Html as Html
import Mation.Core.Dom (DomNode)
import Mation.Core.Patch as Patch
import Mation.Core.Util.WRef (WRef)
import Mation.Core.Util.WRef as WRef


-- | Like `Mation.Run (runApp)` but with event handlers living in a user-specified monad `m`.
-- |
-- | Use of this function is not recommended, for reasons discussed below.
-- | The function is provided anyway for those who are sure they want to use it or are
-- | forced to use it due to integrating with a library or framework that makes use
-- | of a custom monad.
-- |
-- | The custom monad `m` is required to instantiate `MonadUnliftEffect`. This is quite a
-- | stringent requirement, effectively requiring `m` to be a compositions of `IdentityT`
-- | and `ReaderT`s over `Effect`. If you are wondering how the heck you are
-- | supposed to do anything with only `ReaderT`, I recommend reading
-- | [The ReaderT Design Pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/)
-- | from FPComplete.
-- |
-- | ***
-- |
-- | ### Why the stringent `MonadUnliftEffect` constraint?
-- |
-- | Choosing a custom monad `m` decides what monadic context your application's event
-- | handlers will live in. The trouble is that, in general, events really do not play nice
-- | with custom monads.
-- |
-- | For starters, events are dispatched by the javascript runtime. This means that
-- | when an event handler is invoked it is done so from `Effect`, not `m`. Hence
-- | we need some way to execute `m` from within `Effect`; this gives rise
-- | to the `MonadUnliftEffect` constraint.
-- |
-- | We could potentially weaken this constraint to something
-- | like `MonadBaseControl Effect stM` (for some `stM`), but hairy questions
-- | quickly arise from this choice. For example,
-- |
-- | - When an event handler is invoked, from where will it load its initial
-- |   state? To where will it put its resultant state? (In other words, how do
-- |   we ensure that `StateT` works as expected?)
-- |
-- | - Should the monadic state be *entirely* shared between all event handlers?
-- |   (No; that would mean a failure in `EitherT` when handling one event nullifies
-- |   all handlers.) If not, how do we integrate the state of a completed handler
-- |   into the states of all other handlers?
-- |
-- | - If our monad `m` supports genuine concurrency (eg, builds on `Aff`), how
-- |   do we deal with divergent states?
-- |
-- | Beyond this, I suspect that additional, more technical, challenges would
-- | arise internal to the framework from an attempt to weaken `MonadUnliftEffect`.
-- |
-- | Unlifting custom monads to `Effect` *correctly* is
-- | [not](https://www.fpcomplete.com/blog/2017/06/tale-of-two-brackets/)
-- | [an](http://blog.ezyang.com/2012/01/monadbasecontrol-is-unsound/)
-- | [easy](https://hackage.haskell.org/package/unliftio-0.2.24.0#comparison-to-other-approaches)
-- | problem (also relevant:
-- | [this](https://lexi-lambda.github.io/blog/2019/09/07/demystifying-monadbasecontrol/)
-- | and
-- | [this](https://www.yesodweb.com/book/monad-control)),
-- | and until the correct solution is found for mation (if there is one), I'd rather
-- | overconstrain the user than supply a footgun.
-- |
-- | ### Why is using a custom monad not recommended?
-- |
-- | Due to the reasons discussed above, any custom monad `m` must
-- | satisfiy `MonadUnliftEffect`. This in turn entails that `m` must be isomorphic
-- | to `ReaderT env Effect` for some type `env.`
-- |
-- | In other words, we know that the custom monad `m` only acts to carry around
-- | one or more environment objects. Mation already provides a way to carry around
-- | data like this; namely, storing it in component models and/or threading it
-- | through component `view`s. Further, this approach has several advantages
-- | over the `ReaderT` approach:
-- |
-- | - It's more explicit: Threading your environment data through your application
-- |   may be somewhat annoying, but it makes crystal clear how the data moves around
-- |   the app. Contrarily, using `ReaderT` exhibits a certain kind of "action at a
-- |   distance": at the *top* level you supply some `env`, and then event listeners
-- |   very far away in the codebase "automagically" have access to that `env`.
-- |
-- | - It's opt-in instead of opt-out: When composing components, the path
-- |   of least resistance is to to match the underlying
-- |   monad of the child component with that of the parent component.
-- |   When using the `ReaderT` approach for an application environment,
-- |   this means by default providing that environment to the child.
-- |   (Disallowing access requires `hoist`ing the child component `Html`.)
-- |
-- |   Additionally, the path of least resistance when creating a child component
-- |   is to give it a smaller model rather than a larger one.
-- |   When using the explicit threading approach for an application environment
-- |   this means by default *not* providing the child component access to that
-- |   environment. (Providing access requires adding it to the child state.)
-- |
-- | - It's automatically available to the view: State managed in `m` is by
-- |   default invisible to your application view. Hence if you manage, say,
-- |   the login state in `m`, and your view needs to decide depending on that
-- |   state whether to show a "log in" button or a "log out" button, you
-- |   will have extra work to do.
-- |
-- | - Managing readability/writability is easier:
-- |   A `ReaderT` only "natively" supports local modification
-- |   to state. To achieve read/write state, one has to store a mutable ref
-- |   in their `ReaderT` environment.
-- |   If a parent component has read/write access to some state, and wants to
-- |   give their child component readonly access, then they'd have to `hoist`
-- |   the child component and transform the read/write ref into
-- |   some getter `get :: Effect Thing` function.
-- |
-- |   None of this is *bad* but is somewhat awkward. And far less akward using
-- |   explicit threading. State stored in the model is by default read/write,
-- |   and choosing whether a child component has read/write access to some state
-- |   or only readonly access requires simply choosing whether to provide the
-- |   state as a part of the child model or as a parameter to its view.
-- |
-- | ### What should I do instead?
-- |
-- | Assuming you have a choice (ie, are not forced to use a custom monad due
-- | to integrating with some other framework), do the following.
-- |
-- | Have your listeners live in `Effect`. Use
-- | [the ReaderT design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/)
-- | to do everything you'd usually do with a monad stack, except
-- | instead of using `ReaderT` to pass around the environment object,
-- | store it in your application model and explicitly thread it through
-- | your application's component's models/views.
-- |
-- | ### But I want my event handlers to live in `Aff` :(
-- |
-- | Yeah, me too 😞. Asynchronous event handlers are extremely common and
-- | not at all problematic. Just turn them into `Effect` with `launchAff_`
-- | or similar.
-- |
-- | The issue is that `m` decides not only where event handlers live but also
-- | where *fixups* live (see `Mation.Props.Unsafe`). Fixups are allowed to
-- | perform arbitrary modifications to an element after its rendered, such
-- | as manually calling `addEventListener`. It's absolutely crucial that this
-- | work be done synchronously so that the virtual DOM (ie, view) and actual DOM
-- | remain in sync.
-- |
-- | We could potentially allow event listeners to live in `Aff` by
-- | parameterizing `Html` with two monads, one for fixups and one for listeners;
-- | or by "allowing" fixups to live in `Aff` but warning *very loudly* that
-- | they really ought to be synchronous.
-- |
-- | But for so much noise all we'd gain is being able to avoid
-- | writing `launchAff_` at the beginning of our event handlers. Not worth it.
runAppM :: forall m s. MonadUnliftEffect m =>
  { initial :: s
  , render :: s -> Html m s
  , root :: Effect DomNode
  , daemon :: Daemon m s
  } -> m Unit

runAppM args = withRunInEffect \(toEffect :: m ~> Effect) -> do

  let
    -- Render to an VNode instead of an Html
    -- This is unsafe, but during usual usage of the framework should never happen
    renderTo1 :: s -> Effect (VNode (MationT m s))
    renderTo1 = args.render >>> case _ of
      Html [x] -> pure x
      _ -> throw "[mation] Error: Top-level Html value contains either zero nodes or more than one node. Did you produce `mempty`, perhaps, or some result of `<>` or `fold`? Please wrap your application in a container node."

  -- This will eventually hold the real step function, but is
  -- initialized with a dummy
  --
  -- This works because no Mation should ever be executed until
  -- after the first render is complete (which is when this WRef
  -- will be populated)
  --
  -- This is an ugly setup but is necessary to bootstrap the
  -- application
  stepRef :: WRef ((s -> s) -> Effect Unit)
    <- WRef.make (\_ -> pure unit)

  let
    -- Turn a Mation into an Effect
    execMation :: MationT m s ~> Effect
    execMation mat = do
      step <- WRef.read stepRef
      toEffect $ MationT.runMationT mat (step >>> liftEffect)

  -- Render for the first time
  model /\ vNode /\ pruneMap <- do
    let model = args.initial
    vNode <- renderTo1 model
    let patch = Patch.patchOnto
                  { mOldVNode: Nothing
                  , newVNode: Html.hoist1 execMation vNode
                  , mPruneMap: Nothing
                  }
    pruneMap <- args.root >>= patch
    pure $ model /\ vNode /\ pruneMap

  -- Holds current state
  ref <- WRef.make (model /\ vNode /\ pruneMap)

  let
    -- This is the actual step function
    step :: (s -> s) -> Effect Unit
    step endo = do
      oldModel /\ oldVNode /\ oldPruneMap <- WRef.read ref
      let newModel = endo oldModel
      newVNode <- renderTo1 newModel
      let patch = Patch.patchOnto
                    { mOldVNode: Just (Html.hoist1 execMation oldVNode)
                    , newVNode: Html.hoist1 execMation newVNode
                    , mPruneMap: Just oldPruneMap
                    }
      newPruneMap <- args.root >>= patch
      WRef.write (newModel /\ newVNode /\ newPruneMap) ref

  -- Populate the stepRef with the correct value
  WRef.write step stepRef

  -- Start the daemon
  ref # WRef.onChange \_ -> step identity
    -- FIXME: this^ is kind of a hack. The invarant between
    --   the three states is broken temporarily until
    --   we call 'step identity'
  toEffect $ (Daemon.enroot _1 args.daemon) ref


