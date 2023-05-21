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

-- | Simplified version of `runApp`
-- |
-- | Accepts a record containing the following fields:
-- |
-- | - `initial :: s`: Initial model (ie, state) value
-- | - `render :: s -> Html m s`: How to display the application
-- | - `root :: Effect DomNode`: Where to mount the application (see eg `onBody`)
runApp' :: forall s.
  { initial :: s
  , render :: s -> Html Effect s
  , root :: Effect DomNode
  } -> Effect Unit

runApp' args =
  runApp
    { initial: args.initial
    , render: args.render
    , root: args.root
    , daemon: mempty
    }


-- | Run an application.
-- |
-- | Accepts a record containing the following fields:
-- |
-- | - `initial :: s`
-- |
-- |   The initial model value. The type variable uses the character `s`
-- |   for "state"
-- |
-- | - `render :: s -> Html m s`
-- |
-- |   Specifies how to display the application
-- |
-- | - `daemon :: Daemon Effect s`
-- |
-- |   Possibly-long-lived process which has read/write access to
-- |   the application state and runs in parallel with the application
-- |   itself.
-- |
-- |   The daemon is the only thing that can read the state back
-- |   out of a running application.
-- |
-- |   If the daemon changes the application state, the application
-- |   will re-render.
-- |
-- | - `root :: Effect DomNode`
-- |
-- |   Specifies where the application should be mounted. See `onBody`
-- |   and others below.
-- |
-- |   Remarks:
-- |
-- |   - The process of mounting may replace the given `DomNode`. The
-- |     given `DomNode` only guarantees *where* the mounting occurs in
-- |     the DOM
-- |
-- |   - This is an `Effect DomNode`, meaning that the mountpoint can
-- |     theoretically change between frames. If you do change the
-- |     mountpoint, be sure that the new one is equivalent to the old
-- |     one up to details produced by `render`. For various reasons,
-- |     the Mation rendering algorithm is sensitive to this.
runApp :: forall s.
  { initial :: s
  , render :: s -> Html Effect s
  , root :: Effect DomNode
  , daemon :: Daemon Effect s
  } -> Effect Unit
runApp = runAppM


-- | Like `runApp` but with event handlers living in a user-specified monad `m`.
-- |
-- | The monad `m` is required to instantiate `MonadUnliftEffect`. This is quite a
-- | stringent requirement, effectively requiring `m` to be a compositions of `IdentityT`
-- | and `ReaderT`s over `Effect`. If you are wondering how the heck you are
-- | supposed to do anything with only `ReaderT`, I recommend reading
-- | [The ReaderT Design Pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/)
-- | from FPComplete.
-- |
-- | ***
-- |
-- | The `MonadUnliftEffect m` constraint comes from the semantics of event
-- | listeners. Event listeners are invoked by the javascript runtime, which
-- | lives in `Effect`; if we want listeners to live in `m` then we need some
-- | way to execute `m` in `Effect`. Hence, we ask for `MonadUnliftEffect`.
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
-- |   (No; that would mean a failure in `EitherT` by one handler nullifies all
-- |   other handlers.) If not, how do we integrate the state of a completed handler
-- |   into the states of all other handlers?
-- |
-- | - If our monad `m` supports genuine concurrency (ie, builds on `Aff`), how
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



-- | Mount an application on `<body>`. The application will replace `<body>` each render
foreign import onBody :: Effect DomNode

-- | Mount an application as a child of `<body>`
foreign import underBody:: Effect DomNode

-- | Mount an application on `<html>`. The application will replace `<html>` each render
foreign import onHtml :: Effect DomNode

