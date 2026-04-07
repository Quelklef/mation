module Mation.Tests.MultipleListenersOnSameEvent where

{- METADATA START

{
  "name": "Multiple Listeners on Same Event",
  "desc": "Verifies that having multiple listeners registered to the same (node, event name) pair works",
  "specs": [
    "Clicking the button appends \"XY\" to its text"
  ]
}

METADATA END -}

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Core.Refs (Modify)
import Mation.Core.Refs as Refs


type Model = String

initial :: Model
initial = "O"

render :: Model -> E.Html' (Modify Model)
render text =
  E.div
  [ P.addCss "font-family: sans-serif"
  ]
  [ E.button
    [ P.onClick \_ -> M.modify (_ <> "X")
    , P.onClick \_ -> M.modify (_ <> "Y")
    ]
    [ E.text "Click me"
    ]
  , E.text " "
  , E.text text
  ]

main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }
