module Mation.Tests.Textbox where

{- START METADATA

{
  "name": "Textbox",
  "desc": "Basic test of shared state and HTML escaping",
  "specs": [
    "All three textboxes, plus the two lines below, plus the <input> placeholder, should share their text value",
    "The \"As text\" line should render its value as plaintext, and the \"As HTML\" line as raw HTML"
  ]
}

END METADATA -}

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Core.Refs as Refs


type Textbox = String

initialize :: Effect Textbox
initialize = pure "type in me"

renderTextbox :: Textbox -> E.Html' (M.Write Textbox)
renderTextbox str =
  E.div
  []
  [ E.p
    []
    [ E.input [ P.type_ "text", P.value str, P.onInputValue M.write ]
    , E.text " "
    , E.input [ P.type_ "text", P.value str, P.onInputValue M.write ]
    , E.text " "
    , E.input [ P.type_ "text", P.value str, P.onInputValue M.write ]
    ]
  , E.p [] [ E.text "As text: ", E.text str ]
  , E.p [] [ E.text "As HTML: ", E.rawHtml str ]
  , E.p [] [ E.text "As placeholder: ", E.input [ P.placeholder str ] ]
  ]

main :: Effect Unit
main = do
  initial <- initialize
  M.runApp
    { initial
    , render: renderTextbox >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }

