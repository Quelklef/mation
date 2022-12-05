module Mation.Run where

import Mation.Prelude

import Effect.Ref as Ref

import Mation.Mation (Mation)
import Mation.Mation as Mation
import Mation.Html (Html, DOMNode)
import Mation.Html as Html


foreign import getBody :: Effect DOMNode
foreign import mountUnder :: { container :: DOMNode, mountMe :: DOMNode } -> Effect Unit

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

  modelRef <- Ref.new args.initial

  let
    toEff :: Mation m s -> Effect Unit
    toEff mat =
      args.toEffect $
        Mation.runMation mat
          \endo -> do
            Ref.modify_ endo modelRef
            doRender

    doRender :: Effect Unit
    doRender = do
      model <- Ref.read modelRef
      node <- Html.render toEff (args.render model)
      root <- args.root
      mountUnder { container: root, mountMe: node }

  doRender

