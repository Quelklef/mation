module Mation.Core.Run where

import Mation.Core.Prelude

import Effect.Exception (throw)

import Mation.WRef (WRef)
import Mation.WRef as WRef
import Mation.Core.Mation (Mation)
import Mation.Core.Mation as Mation
import Mation.Core.Html (Html (..), VNode)
import Mation.Core.Dom (DomNode)
import Mation.Core.Patch as Patch

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
    , kickoff: Mation.mkNoop
    , withState: mempty
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
-- | - `kickoff :: Mation m s`
-- |
-- |   An initial `Mation` to execute
-- |
-- | - `withState :: WRef s -> Effect Unit`
--
-- FIXME: pretty sure the type 'type T s = WRef s -> Effect Unit` would
--        admit an enroot function? Maybe we should give this abstraction
--        a proper name and write it an enroot function?
--
-- |
-- |   Supplies the application state `WRef` to a callback. This
-- |   is the only way the state can be read back out of a running
-- |   applicaiton.
-- |
-- |   Changing the state in the provided reference will invoke
-- |   an application re-render.
-- |
-- |   Existence of `withState` makes `kickoff` redundant, because
-- |   one could use `runMation` and `toEffect` to turn the kickoff
-- |   mation into an `Effect Unit`. `kickoff` is provided as a less
-- |   powerful but more convenient alternative to `withState`.
-- |   Note that `kickoff` executes before `withState`.
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
-- |   Note that you are only asked to provide an `m Unit -> Effect Unit`
-- |   as opposed to, for instance, an `forall a. m a -> Effect a`. This
-- |   means that, for instance, `toEffect` can be asynchronous. Beware
-- |   that if `toEffect` *is* async, that means that a mation executed
-- |   during one frame may still be running and updating the application
-- |   state despite the application having progressed to a new frame.
runApp :: forall m s. MonadEffect m =>
  { initial :: s
  , render :: s -> Html m s
  , root :: Effect DomNode
  , kickoff :: Mation m s
  , withState :: WRef s -> Effect Unit
  , toEffect :: m Unit -> Effect Unit
  } -> Effect Unit

runApp args = do

  let
    -- Render to an VNode instead of an Html
    -- This is unsafe, but during usual usage of the framework should never happen
    renderTo1 :: s -> Effect (VNode (Mation m s))
    renderTo1 = args.render >>> case _ of
      Html [x] -> pure x
      _ -> throw "Unexpected Html value: toplevel node contains more than one value. Did you `<>` or `fold` some top-level Html values? Please wrap them in some container element."

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

  -- Execute kickoff
  execMation args.kickoff

  -- Execute withState
  ref # WRef.onChange \_ -> step identity
    -- FIXME: this^ is kind of a hack. The invarant between
    --   the three states is broken temporarily until
    --   we call 'step identity'
  args.withState (WRef.mkView _1 ref)



-- | Mount an application as a child of `<body>`
foreign import onBody :: Effect DomNode

-- | Mount an application on `<body>`. The application will replace `<body>` each render
foreign import underBody:: Effect DomNode

-- | Mount an application on `<html>`. The application will replace `<html>` each render
foreign import onHtml :: Effect DomNode

