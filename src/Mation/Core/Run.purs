module Mation.Core.Run where

import Mation.Core.Prelude

import Effect.Exception (throw)

import Mation.Core.Mation (Mation)
import Mation.Core.Mation as Mation
import Mation.Core.Daemon (Daemon)
import Mation.Core.Daemon as Daemon
import Mation.Core.Html (Html (..), VNode)
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
    , toEffect: identity
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
-- | - `daemon :: Daemon m s`
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
-- |
-- | - `toEffect :: m Unit -> Effect Unit`
-- |
-- |   Specifies how to execute the custom monad `m`.
-- |
-- |   Note the type of the function, `m Unit -> Effect Unit`. We do not
-- |   require `toEffect` to be satisfy any conditions, such as being
-- |   a monad morphism (see [1]) or even just a natural
-- |   transformation (ie, having type `forall a. m a -> n a`),
--
-- Remark: There are various `hoist` functions in this codebase which
-- are similar to `toEffect` insofar as they transform an underlying
-- monad, but differ in that they *do* require a monad morphism. The
-- rationale behind this difference is that `toEffect` is performed
-- at the very top of the program, meaning that any failure to respect
-- the monad structure is "less bad" as we only actually call `toEffect`
-- a few times, and those callsites are known.
--
-- |
-- |   This means that, for example, functions such
-- |   as `launchAff_ :: Aff Unit -> Effect Unit` can be supplied as
-- |   values for `toEffect`
-- |
-- |   However, it is usually in the user's best interest to choose
-- |   a `toEffect` which *is* a monad morphism (specialized to `Unit`).
-- |   Currently `toEffect` is called once to
-- |   execute the given `daemon` and then once per application frame
-- |   to execute the invoked event handler. These details are
-- |   irrelevant if `toEffect` is a monad morphism, but can have subtle
-- |   consequences if it is not. For instance, if `toEffect`
-- |   is `launchAff_`, then mations from one frame may still be in
-- |   progress when an event handler from the next frame is invoked.
-- |
-- |   [1\]:
-- |     A monad morphism is a function `f :: forall a. m a -> n a`
-- |     between `Monad`s `m` and `n` and mapping monad operations
-- |     in `m` to monad operations in `n`; ie, satisfying both
-- |     `f (return x) = return x` and `f (m >>= g) = f m >>= (f . g)`.
-- |     This condiiton ensures that monad operations performed
-- |     within `m` are not mangled by `f` when mapping into `n`.
runApp :: forall m s. MonadEffect m =>
  { initial :: s
  , render :: s -> Html m s
  , root :: Effect DomNode
  , daemon :: Daemon m s
  , toEffect :: m Unit -> Effect Unit
  } -> Effect Unit

runApp args = do

  let
    -- Render to an VNode instead of an Html
    -- This is unsafe, but during usual usage of the framework should never happen
    renderTo1 :: s -> Effect (VNode (Mation m s))
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
    <- WRef.new (\_ -> pure unit)

  let
    -- Turn a Mation into an Effect
    execMation :: Mation m s -> Effect Unit
    execMation mat = do
      step <- WRef.get stepRef
      args.toEffect $ Mation.runMation mat (step >>> liftEffect)

  -- Render for the first time
  model /\ vNode /\ pruneMap <- do
    let model = args.initial
    vNode <- renderTo1 model
    let patch = Patch.patchOnto
                  { mOldVNode: Nothing
                  , newVNode: execMation <$> vNode
                  , mPruneMap: Nothing
                  }
    pruneMap <- args.root >>= patch
    pure $ model /\ vNode /\ pruneMap

  -- Holds current state
  ref <- WRef.new (model /\ vNode /\ pruneMap)

  let
    -- This is the actual step function
    step :: (s -> s) -> Effect Unit
    step endo = do
      oldModel /\ oldVNode /\ oldPruneMap <- WRef.get ref
      let newModel = endo oldModel
      newVNode <- renderTo1 newModel
      let patch = Patch.patchOnto
                    { mOldVNode: Just (execMation <$> oldVNode)
                    , newVNode: execMation <$> newVNode
                    , mPruneMap: Just oldPruneMap
                    }
      newPruneMap <- args.root >>= patch
      WRef.set (newModel /\ newVNode /\ newPruneMap) ref

  -- Populate the stepRef with the correct value
  WRef.set step stepRef

  -- Start the daemon
  ref # WRef.onChange \_ -> step identity
    -- FIXME: this^ is kind of a hack. The invarant between
    --   the three states is broken temporarily until
    --   we call 'step identity'
  args.toEffect $ (Daemon.enroot _1 args.daemon) ref



-- | Mount an application as a child of `<body>`
foreign import onBody :: Effect DomNode

-- | Mount an application on `<body>`. The application will replace `<body>` each render
foreign import underBody:: Effect DomNode

-- | Mount an application on `<html>`. The application will replace `<html>` each render
foreign import onHtml :: Effect DomNode

