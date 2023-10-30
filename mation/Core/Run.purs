module Mation.Core.Run where

import Mation.Core.Prelude

import Effect.Exception (throw)

import Mation.Core.Html (Html (..), VNode, fixVNode)
import Mation.Core.Html as Html
import Mation.Core.Dom (DomNode)
import Mation.Core.Patch as Patch
import Mation.Core.Refs as Refs
import Mation.Core.Refs (ReadWriteL, ReadWrite)


type Daemon m s = ReadWriteL m s -> m Unit

type Daemon' s = Daemon Effect s


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
-- | handlers will live in. The trouble is that events are asynchronous, which
-- | does not paly well with many kinds of monads.
-- |
-- | Events are dispatched by the javascript runtime. This means that
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
-- | Interpolating custom monads with `Effect` *correctly* is
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
-- | onsome environment data. Mation already provides a way to carry around
-- | data like this; namely, storing it in component models and/or threading it
-- | through component `view`s. Further, this "threading" approach has several
-- | advantages over the `ReaderT` approach:
-- |
-- | - It's more explicit: Threading your environment data through your application
-- |   may be somewhat annoying, but it makes crystal clear how the data moves around
-- |   the app. Contrarily, using `ReaderT` exhibits a certain kind of "action at a
-- |   distance": at the *top* of your application you supply some `env`, and then event
-- |   listeners far away in the codebase "automagically" have access to that `env`.
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
-- |   When using the threading approach for an application environment
-- |   this means by default *not* providing the child component access to that
-- |   environment. (Providing access requires adding it to the child state.)
-- |
-- | - The environment is automatically available to the view: State managed in
-- |   a custom monad `m` is by default visible to your event handlers but
-- |   invisible to your application view. Hence if you manage, say,
-- |   the login state as part of your environment in `m`, and if your view
-- |   needs to know if the user is logged in, you will have extra work to do.
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
-- | not at all problematic. Just use `launchAff_` or similar to discharge
-- | your handlers into `Effect`.
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
--
-- FIXME: add runAppML variant where 'render' is given a ReadWriteL
runAppM :: forall m s. MonadUnliftEffect m =>
  { initial :: s
  , render :: s -> Html m (ReadWrite m s)
  , root :: Effect DomNode
  , daemon :: Daemon m s
  } -> m Unit

runAppM args = withRunInEffect \(toEffect :: m ~> Effect) -> do

  let
    -- Render to a single VNode (instead of an entire Html)
    -- This is unsafe, but during usual usage of the framework should never happen
    renderTo1 :: s -> Effect (VNode m (ReadWrite m s))
    renderTo1 = args.render >>> case _ of
      Html [x] -> pure x
      _ -> throw "[mation] Error: Top-level Html value contains either zero nodes or more than one node. Did you produce `mempty`, perhaps, or some result of `<>` or `fold`? Please wrap your application in a container node."


  -- Tracks user application state
  (stateRef :: ReadWriteL Effect s) <- Refs.make args.initial

  -- Tracks Mation internal state
  (vNodeRef :: ReadWrite Effect (Maybe (VNode Effect Unit))) <- Refs.make Nothing
  (pruneMapRef :: ReadWrite Effect (Maybe _)) <- Refs.make Nothing

  -- On change to state, re-render application
  stateRef # Refs.onChange \newState -> do
    mOldVNode <- vNodeRef # Refs.read
    (newVNode :: VNode m (ReadWrite m s)) <- renderTo1 newState
    let (newVNode' :: VNode Effect Unit) =
            newVNode
            # fixVNode (stateRef # Refs.downcast # Refs.hoist liftEffect)
            # Html.hoist1 toEffect
    mOldPruneMap <- pruneMapRef # Refs.read
    let patch = Patch.patchOnto
                    { mOldVNode
                    , newVNode: newVNode'
                    , mPruneMap: mOldPruneMap
                    }
    newPruneMap <- args.root >>= patch
    vNodeRef # Refs.write (Just newVNode')
    pruneMapRef # Refs.write (Just newPruneMap)
    pure unit

  -- Induce initial render
  stateRef # Refs.modify identity

  -- Start the daemon
  toEffect $ args.daemon (stateRef # Refs.hoistWithIso liftEffect toEffect)


