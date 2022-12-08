module Mation.Core.Run where

import Mation.Core.Prelude

import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Exception (throw)

import Mation.Core.Mation (Mation)
import Mation.Core.Mation as Mation
import Mation.Core.Html (Html (..), Html1)
import Mation.Core.Dom (DomNode)
import Mation.Core.Patch as Patch

-- | Simplified version of @runApp@
runApp' :: forall s.
  { initial :: s
      -- ^ Initial model value (ie state)
  , render :: s -> Html Effect s
      -- ^ How to display the model
  , root :: Effect DomNode
      -- ^
      -- Where should the application be mounted?
      --
      -- Try eg. @useBody@ to mount on <body>
      --
      -- Note that the supplied DOM node may be replaced on each render
  } -> Effect Unit

runApp' args =
  runApp
    { initial: args.initial
    , render: args.render
    , root: args.root
    , kickoff: Mation.mkNoop
    , listen: \_ -> pure unit
    , toEffect: identity
    }


-- | Run an application
runApp :: forall m s. MonadEffect m =>

  { initial :: s
      -- ^
      -- Initial model value
      --
      -- The type variable is 's' for 'state'

  , render :: s -> Html m s
      -- ^
      -- How to display the model value

  , root :: Effect DomNode
      -- ^
      -- Where should the application be mounted?
      --
      -- Try eg. @useBody@ to mount on <body>
      --
      -- Note that the supplied DOM node may be replaced on each render
      --
      -- Note that this is an @Effect@, meaning that the mount point
      -- can change. However, the rendering algorithm does not
      -- respect arbitrary mountpoint modification. For various reasons,
      -- it expects the mountpoint to contain something that looks
      -- reasonably close to the result of the previous render.
      -- Practically this means that you cannot re-mount a running
      -- application to new node DOM node; you either move the DOM node
      -- itself or .cloneNode() it.

  , kickoff :: Mation m s
      -- ^
      -- An initial mation to execute

  , listen :: s -> Effect Unit
      -- ^
      -- Called every time the state changes
      --
      -- This is the ONLY place in the codebase where the state
      -- can be read back out of a running application.
      -- Usually reading state should not be necessary, and should
      -- be done with caution.

  , toEffect :: forall a. m a -> Effect a
      -- ^
      -- Turn the custom monad into an Effect

  } -> Effect Unit

runApp args = do

  let
    -- Render to an Html1 instead of an Html
    -- This is unsafe, but during usual usage of the framework should never happen
    renderTo1 :: s -> Effect (Html1 m s)
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
    -- Required by patchOnto
    toEff :: Mation m s -> Effect Unit
    toEff mat = do
      step <- Ref.read stepRef
      args.toEffect $ Mation.runMation mat step

  -- Render for the first time
  model /\ html <- do
    let model = args.initial
    html <- renderTo1 model
    let patch = Patch.patchOnto { toEff, old: Nothing, new: html }
    args.root >>= patch
    args.listen model
    pure $ model /\ html

  -- Holds current model & rendered HTML
  ref <- Ref.new (model /\ html)

  let
    -- This is the actual step function
    step :: (s -> s) -> Effect Unit
    step endo = do
      oldModel /\ oldHtml <- Ref.read ref
      let newModel = endo oldModel
      newHtml <- renderTo1 newModel
      Ref.write (newModel /\ newHtml) ref
      let patch = Patch.patchOnto { toEff, old: Just oldHtml, new: newHtml }
      args.root >>= patch
      args.listen newModel

  -- Populate the stepRef with the correct value
  Ref.write step stepRef


-- |
--
-- Used to mount an application on <body>
--
-- Application will be rendered as a child of <body>
foreign import useBody :: Effect DomNode

-- |
--
-- Used to mount an application on <html>
--
-- Application will replace <html> each render
foreign import useHtml :: Effect DomNode
