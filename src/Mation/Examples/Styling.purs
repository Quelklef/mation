module Mation.Examples.Styling where

import Mation.Core.Prelude

import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Selectors as Sel
import Mation.Selectors ((>>))

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
      (S.on Sel.hover)

  , mkExample
      "I style when the page is ≤800px tall"
      (S.on $ Sel.media "(max-height: 800px)")

  , mkExample
      "I style my children"
      (S.on Sel.children)

  , mkExample
      "I style my children when they are hovered"
      (S.on $ Sel.children >> Sel.hover)

  , mkExample
      "I style my children when I am hovered"
      (S.on $ Sel.hover >> Sel.children)

  , mkExample
      "I style my children when the page is ≤800px tall"
      (S.on $ Sel.children >> Sel.media "(max-height: 800px)")
      
  ]

  where

  mkExample text condition =
    E.div
    []
    [ E.span
      [ P.style'
        [ S.display "inline-block"
        , S.cursor "pointer"
        , S.padding "1em"
        , S.border "1px solid black"
        , condition
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
