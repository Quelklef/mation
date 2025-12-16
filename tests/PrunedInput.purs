module Mation.Tests.PrunedInput where

{- METADATA START

{
  "name": "Pruned Input",
  "desc": "Regression test for pruned inputs",
  "specs": [
    "Typing in both <input>s should work without stuttering",
    "The inputted text should appear next to the <input> in real-time"
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
initial = "text"

render :: Model -> E.Html' (Modify Model)
render text =
  E.div
  [ P.addCss "font-family: sans-serif"
  ]
  [ E.p [] [ E.text "Non-pruned: ", viewInput text ]
  , E.p [] [ E.text "Pruned: ", E.pruneUeq "the-input" viewInput text ]
  , E.p [] [ E.text "Text: ", E.text text ]
  ]


viewInput :: String -> E.Html' (Modify String)
viewInput text =
  E.input
  [ P.value text
  , P.onInputValue \newText -> Refs.write newText
  ]


main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }
