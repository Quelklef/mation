
-- | A type for Mation components
-- |
-- | **Use of this module is entirely optional**.
-- | One can componentize their mation code without using the `Component` type
-- |
-- | The intended use of the `Component` type is to make using components
-- | more uniform and streamlined by prescribing a specific shape.
-- | The tradeoff is that using `Component` reduces flexibility.
-- |
-- | **This module is experimental. It may change at any time.**

module Mation.Experimental.Component where

import Mation.Core.Prelude

import Data.Lens (Lens)
import Data.Symbol (class IsSymbol, reflectSymbol)

import Mation (Daemon, Daemon')
import Mation as M
import Mation.Elems (Html)
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Core.Refs as Refs


-- | A mation component
-- |
-- | Takes several type parameters:
-- |
-- | - `model`, the component model
-- |
-- | - `sup`, the component's so-called "supermodel"
-- |
-- |   This is state that the component has access to beyond its model
-- |
-- | - `m`, the ambient monad
newtype Component m sup model = Com
  { init :: Effect model
  , daemon :: Daemon Effect (sup /\ model)
  , view :: sup /\ model -> Html m (M.Modify (sup /\ model))
  }


-- | Create a component
-- |
-- | - The component will automatically be labelled with the given `name`
-- |   via `Mation.Props (remark)`
mkComponent :: forall name m sup model. IsSymbol name =>
  { name :: Proxy name
  , init :: Effect model
  , daemon :: Daemon Effect (sup /\ model)
  , view :: sup /\ model -> Html m (M.Modify (sup /\ model))
  } -> Component m sup model

mkComponent { name, init, daemon, view } = Com
  { init
  , daemon
  , view: \s ->
      E.span
      [ P.addStyles [ S.display "contents" ]
      , P.remark (reflectSymbol name)
      ]
      [ view s
      ]
  }


initializeC :: forall m sup model. Component m sup model -> Effect model
initializeC (Com { init }) = init

daemonC :: forall m sup model. Component m sup model -> Daemon' (sup /\ model)
daemonC (Com { daemon }) = daemon

viewC :: forall m sup model. Component m sup model -> (sup /\ model -> Html m (M.Modify (sup /\ model)))
viewC (Com { view }) = view


mkParent1 :: forall
  name m
  sup model
  sup1 model1
  .
  IsSymbol name =>
  Functor m =>

      -- | Parent component name
  { name :: Proxy name
      -- | Accepts as input the result of initializing the child
  , init :: model1 -> Effect model
      -- | Initialize the parent
  , daemon :: Daemon Effect (sup /\ model)
      -- | Accepts as input the result of the child's view function
  , view :: Html m (M.Modify (sup /\ model)) -> (sup /\ model -> Html m (M.Modify (sup /\ model)))
      -- | Child specification
  , child1 :: { component :: Component m sup1 model1, at :: BoxLens' (sup /\ model) (sup1 /\ model1) }
  } -> Component m sup model

mkParent1 { name, init, daemon, view, child1 } =
  mkComponent
    { name
    , init: do
        model1 <- initializeC child1.component
        init model1
    , daemon: \ref -> do
        daemonC child1.component (ref # Refs.focusWithLens child1At)
        daemon ref
    , view: \(sup /\ model) ->
        let
          childViewed :: Html m (M.Modify (sup1 /\ model1))
          childViewed = viewC child1.component ((sup /\ model) ^. child1At)
        in view (cmap (M.focusWithLens child1At) childViewed) (sup /\ model)
    }

  where

  child1At :: Lens' (sup /\ model) (sup1 /\ model1)
  child1At = unBox child1.at


-- Used to circumvent some typesystem difficulties ...
newtype BoxLens s t a b = BoxLens (Lens s t a b)
type BoxLens' s a = BoxLens s s a a

unBox :: forall s t a b. BoxLens s t a b -> Lens s t a b
unBox (BoxLens l) = l
