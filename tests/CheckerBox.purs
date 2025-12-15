module Mation.Tests.CheckerBox where

{- BEGIN METADATA

{
  "name": "Checker Box",
  "desc": "Basic test of shared state and monoidal HTML",
  "specs": [
    "Clicking any checkbox should invert them all"
  ]
}

END METADATA -}

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Core.Refs as Refs


type Model = Boolean

initialize :: Effect Model
initialize = pure false

render :: Model -> E.Html' (M.Modify Model)
render checked =
  E.div [] $
    [ E.input [ P.type_ "checkbox", P.checked      checked , P.onInput \_ -> M.modify not ]
    , E.input [ P.type_ "checkbox", P.checked (not checked), P.onInput \_ -> M.modify not ]
    ]
    `power` 15

main :: Effect Unit
main = do
  initial <- initialize
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }

