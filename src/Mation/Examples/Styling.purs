module Mation.Examples.Styling where

import Mation.Core.Prelude

import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Selectors as Sel
import Mation.Selectors ((#>>), (#<>))

type Model = Unit

initial :: Model
initial = unit

render :: forall m. Model -> E.Html m Model
render _model =
  E.div
  [ P.style "display: flex; flex-direction: column; gap: 1em;"
  ]
  [ mkExample
      "I style on hover"
      Sel.hover

  , mkExample
      "I style when the page is ≤800px tall"
      (Sel.media "(max-height: 800px)")

  , mkExample
      "I style my children"
      Sel.children

  , mkExample
      "I style my children when they are hovered"
      (Sel.children #>> Sel.hover)

  , mkExample
      "I style my children when I am hovered"
      (Sel.hover #>> Sel.children)

  , mkExample
      "I style my children when the page is ≤800px tall"
      (Sel.children #>> Sel.media "(max-height: 800px)")

  , E.hr [ P.style "width: 100%" ]

  , mkExample
      "I style: my first child; my last child when I am not hovered; my middle child when it is hovered or when the page is ≤800px tall; myself when the page is ≤700px tall"
      $ (Sel.children #>> (Sel.firstChild #<> (Sel.nthChild "2" #>> (Sel.hover #<> Sel.media "(max-height: 800px)"))))
          #<> (Sel.not ":hover" #>> Sel.children #>> Sel.lastChild)
          #<> (Sel.this #>> Sel.media "(max-height: 700px)")
      
  ]

  where

  mkExample text selector =
    E.div
    []
    [ E.span
      [ P.style'
        [ S.display "inline-block"
        , S.cursor "pointer"
        , S.padding "1em"
        , S.border "1px solid black"
        , S.on selector
            [ S.background "rgb(230, 230, 230)"
            , S.textDecoration "underline"
            ]
        ]
      ]
      [ E.span [] [ E.text "⋆⋆⋆" ]
      , E.text " "
      , E.span [] [ E.text text ]
      , E.text " "
      , E.span [] [ E.text "⋆⋆⋆" ]
      ]
    ]
