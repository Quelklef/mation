module Mation.Tests.ComponentsApi where

{- METADATA START

{
  "name": "Components API",
  "desc": "Playground for the experimental Components API",
  "specs": []
}

METADATA END -}

import Mation.Core.Prelude

import Data.Lens (lens)

import Mation as M
import Mation (Daemon)
import Mation.Elems as E
import Mation.Props as P
import Mation.Core.Refs as Refs
import Mation.Experimental.Component as Com
import Mation.Lenses (field, (×))


type Model =
  { string :: String
  , caps :: Boolean
  }

stringComponent :: Com.Component Effect Unit String
stringComponent =
  Com.mkComponent
  { name: Proxy :: Proxy "left rootComponent"
  , init: pure "meow meow meow"
  , daemon: \_ -> pure unit
  , view: \(_ /\ s) ->
      E.input
      [ P.onInputValue \v -> M.modify (_2 .~ v)
      , P.value s
      ]
  }

rootComponent :: Com.Component Effect Unit Model
rootComponent =
  Com.mkParent1
  { name: Proxy :: Proxy "Component api example"
  , init: \string -> pure { string, caps: false }
  , daemon: \_ -> pure unit
  , view: \stringCom (_ /\ { string, caps }) ->
      E.div
      []
      [ stringCom
      , E.text " "
      , E.label [ P.for "caps" ] [ E.text "caps" ]
      , E.input
        [ P.type_ "checkbox"
        , P.id "caps"
        , P.checked caps
        , P.onInputValue \_ -> M.modify (_2 <<< field @"caps" %~ not)
        ]
      , E.text " → "
      , E.text (if caps then toUpperCase string else string)
      ]
  , child1:
    { component: stringComponent
    , at: Com.BoxLens (_1 × _2 <<< field @"string")
    }
  }

foreign import toUpperCase :: String -> String


daemon :: Daemon Effect Model
daemon ref = do
  (Com.daemonC rootComponent) (ref # Refs.focusWithLens (_unit × identity))

_unit :: forall x. Lens' x Unit
_unit = lens (const unit) (\v _ -> v)

render :: Model -> E.Html' (M.Modify Model)
render model =
  Com.viewC rootComponent (unit /\ model)
  # cmap (M.focusWithLens (_unit × identity))


main :: Effect Unit
main = do
  M.runApp
    { initial: { string: "meow meow meow", caps: true }
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: daemon
    }

