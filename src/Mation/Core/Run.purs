module Mation.Core.Run where

import Mation.Core.Prelude

import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Exception (throw)

import Mation.Core.Mation (Mation)
import Mation.Core.Mation as Mation
import Mation.Core.Html (Html (..), Html1, DOMNode)
import Mation.Core.Patch as Patch

foreign import useBody :: Effect DOMNode

-- FIXME: parameterize over DOMNode type, because I don't want to have to choose
--        a dom FFI library
runApp :: forall m s. MonadEffect m =>
  { initial :: s
  , render :: s -> Html m s
  , kickoff :: Mation m s
  , toEffect :: forall a. m a -> Effect a
  , root :: Effect DOMNode
  } -> Effect Unit

runApp args = do

  let
    -- Render to an Html1 instead of an Html
    -- This is unsafe, but during usual usage of the framework should never happen
    renderTo1 :: s -> Effect (Html1 m s)
    renderTo1 = args.render >>> case _ of
      Html [x] -> pure x
      _ -> throw "Unexpected Html value: toplevel node contains more than one value. Did you `<>` or `fold` some top-level Html values? Please wrap them in a <div>!"

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

  Ref.write step stepRef

