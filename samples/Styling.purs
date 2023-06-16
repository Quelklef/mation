module Mation.Samples.Styling where

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
  [ P.addCss "display: flex; flex-direction: column; gap: 1em;"
  ]
  [ mkSelectorExample
      "I style on hover"
      Sel.hover

  , mkSelectorExample
      "I style when the page is ≤800px tall"
      (Sel.media "(max-height: 800px)")

  , mkSelectorExample
      "I style my children"
      Sel.children

  , mkSelectorExample
      "I style my children when they are hovered"
      (Sel.children #>> Sel.hover)

  , mkSelectorExample
      "I style my children when I am hovered"
      (Sel.hover #>> Sel.children)

  , mkSelectorExample
      "I style my children when the page is ≤800px tall"
      (Sel.children #>> Sel.media "(max-height: 800px)")

  , mkSelectorExample
      "I style: my first child; my last child when I am not hovered; my middle child when it is hovered or when the page is ≤800px tall; myself when the page is ≤700px tall"
      $ (Sel.children #>> (Sel.firstChild #<> (Sel.nthChild "2" #>> (Sel.hover #<> Sel.media "(max-height: 800px)"))))
          #<> (Sel.not ":hover" #>> Sel.children #>> Sel.lastChild)
          #<> (Sel.this #>> Sel.media "(max-height: 700px)")

  , E.hr [ P.addCss "width: 100%" ]

  , mkPreludeExample mempty
  , mkPreludeExample (S.fontWeight "bold")
      
  ]

  where

  mkSelectorExample text selector =
    E.div
    []
    [ E.span
      [ P.addStyles
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

  mkPreludeExample extraStyle =
    E.div
    [ P.addStyles
      [ S.withPrelude
          (fold
            [ "@keyframes my-animation {"
            , "   0% { background-color: hsla(0  , 35%, 50%, .5); }"
            , "  50% { background-color: hsla(100, 35%, 50%, .5); }"
            , " 100% { background-color: hsla(0  , 35%, 50%, .5); }"
            , "}"
            ])
          (S.animation "my-animation 2s infinite")
      , S.padding ".5em 1em"
      , extraStyle
      ]
    ]
    [ E.text "I have an animation!"
    ]

