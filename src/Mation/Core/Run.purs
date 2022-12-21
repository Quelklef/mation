module Mation.Core.Run where

import Mation.Core.Prelude

import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Exception (throw)

import Mation.Core.Mation (Mation)
import Mation.Core.Mation as Mation
import Mation.Core.Html (Html (..), VNode)
import Mation.Core.Dom (DomNode)
import Mation.Core.Patch as Patch

-- | Simplified version of `runApp`
runApp' :: forall s.

    -- | Initial model value (ie state)
  { initial :: s

    -- | How to display the model
  , render :: s -> Html Effect s

    -- | Where should the application be mounted?
    -- |
    -- | Try eg. `onBody` to mount on <body>
    -- |
    -- | Note that the supplied DOM node may be replaced on each render
  , root :: Effect DomNode

  } -> Effect Unit

runApp' args =
  runApp
    { initial: args.initial
    , render: args.render
    , root: args.root
    , kickoff: Mation.mkNoop
    , listen: \_ -> pure Nothing
    , toEffect: identity
    }


-- | Run an application
runApp :: forall m s. MonadEffect m =>

    -- | Initial model value
    -- |
    -- | The type variable is 's' for 'state'
  { initial :: s

    -- | How to display the model value
    -- FIXME: docstrings on record fields aren't rendering :(
  , render :: s -> Html m s

    -- | Where should the application be mounted?
    -- |
    -- | Try eg. `onBody` to mount on <body>
    -- |
    -- | Note that the supplied DOM node may be replaced on each render
    -- |
    -- | Note that this is an `Effect`, meaning that the mount point
    -- | can change. However, the rendering algorithm does not
    -- | respect arbitrary mountpoint modification. For various reasons,
    -- | it expects the mountpoint to contain something that looks
    -- | reasonably close to the result of the previous render.
    -- | Practically this means that you cannot re-mount a running
    -- | application to new node DOM node; you either move the DOM node
    -- | itself or .cloneNode() it.
  , root :: Effect DomNode

    -- | An initial `Mation` to execute
  , kickoff :: Mation m s

    -- | Called every time the state changes
    -- |
    -- | This is the only way the state can be read back out of a running
    -- | application.
    --
    -- FIXME: we need a real subscriptions / "parallel agents" api
  , listen :: { old ::s, new :: s } -> Effect (Maybe (Mation m s))

    -- | Turn the custom monad into an Effect
    -- |
    -- | This will be used to run values of type `Mation m s`. Note that
    -- | you are only required to handle `m Unit` values, not arbitrary
    -- | `m a` values. This means (among other things) that `toEffect` can
    -- | be asynchronous.
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
  -- after the first render is complete (which is when this Ref
  -- will be populated)
  --
  -- This is an ugly setup but is necessary to bootstrap the
  -- application
  stepRef :: Ref ((s -> s) -> Effect Unit)
    <- Ref.new (\_ -> pure unit)

  let
    -- Turn a Mation into an Effect
    execMation :: Mation m s -> Effect Unit
    execMation mat = do
      step <- Ref.read stepRef
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
  ref <- Ref.new (model /\ vNode /\ pruneMap)

  let
    -- This is the actual step function
    step :: (s -> s) -> Effect Unit
    step endo = do
      oldModel /\ oldVNode /\ oldPruneMap <- Ref.read ref
      let newModel = endo oldModel
      newVNode <- renderTo1 newModel
      let patch = Patch.patchOnto
                    { mOldVNode: Just (execMation <$> oldVNode)
                    , newVNode: execMation <$> newVNode
                    , mPruneMap: Just oldPruneMap
                    }
      newPruneMap <- args.root >>= patch
      Ref.write (newModel /\ newVNode /\ newPruneMap) ref
      args.listen { old: oldModel, new: newModel }
        >>= case _ of Nothing -> pure unit
                      Just mat -> execMation mat

  -- Populate the stepRef with the correct value
  Ref.write step stepRef

  execMation args.kickoff


-- | Mount an application as a child of `<body>`
foreign import onBody :: Effect DomNode

-- | Mount an application on `<body>`. The application will replace `<body>` each render
foreign import underBody:: Effect DomNode

-- | Mount an application on `<html>`. The application will replace `<html>` each render
foreign import onHtml :: Effect DomNode

