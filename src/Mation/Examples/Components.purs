module Mation.Examples.Components where

import Prelude
import Effect (Effect)
import Type.Proxy (Proxy (..))
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.String.Common (toUpper)

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S


{-

Example for component composition

Each component gets its own model and its own render
function. We then embed them in the page using E.enroot

-}


type TextboxModel = String

renderTextbox :: TextboxModel -> E.Html' TextboxModel
renderTextbox model =
  E.div
  []
  [ E.p
    []
    [ E.input
      [ P.onInput' \value -> M.mkPure (\_oldValue -> value)
      , P.value model
      ]
    ]
  , E.p
    []
    [ E.text $ "Capitalized: " <> toUpper model
    ]
  ]


type CheckboxModel = Boolean

renderCheckbox :: CheckboxModel -> E.Html' CheckboxModel
renderCheckbox model =
  E.div
  []
  [ E.p
    []
    [ E.input
      [ P.onInput' \_ -> M.mkPure not
      , P.checked model
      , P.type_ "checkbox"
      , P.id "the-checkbox"
      ]
    , E.label
      [ P.for "the-checkbox"
      ]
      [ E.text $ " The box is " <> if model then "checked" else "not checked"
      ]
    ]
  ]


type Model =
  { textbox :: TextboxModel
  , checkbox :: CheckboxModel
  }

initial :: Model
initial =
  { textbox: ""
  , checkbox: false
  }

render :: Model -> E.Html' Model
render model =
  E.div
  [ P.style'
    [ S.display "flex"
    , S.gap "1em"
    , S.alignItems "flex-start"
    ]
  ]
  [ stylesheet
  , E.div
    [ componentStyle ]
    [ E.p [] [ E.text "Textbox component" ]
    , E.hr [ hrStyle ]
    , E.enroot _textbox (renderTextbox model.textbox)
    ]
  , E.div
    [ componentStyle ]
    [ E.p [] [ E.text "Checkbox component" ]
    , E.hr [ hrStyle ]
    , E.enroot _checkbox (renderCheckbox model.checkbox)
    ]
  ]

  where

  stylesheet :: forall m s. E.Html m s
  stylesheet =
    E.style
    []
    [ E.text "p { margin: .5em 0; }"
    ]

  componentStyle :: forall m s. P.Prop m s
  componentStyle =
    P.style'
    [ S.border "1px solid black"
    , S.padding "1em"
    ]

  hrStyle :: forall m s. P.Prop m s
  hrStyle =
    P.style'
    [ S.margin ".5em 0 1em 0"
    ]

  _textbox :: Lens' Model TextboxModel
  _textbox = prop (Proxy :: Proxy "textbox")

  _checkbox :: Lens' Model CheckboxModel
  _checkbox = prop (Proxy :: Proxy "checkbox")


main :: Effect Unit
main = M.runApp' { initial, render, root: M.underBody }
