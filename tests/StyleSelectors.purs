module Mation.Tests.StyleSelectors where

{- BEGIN METADATA

{
  "name": "Style Selectors",
  "desc": "Tests style selectors",
  "specs": [
    "Elements should be styled as their text indicates"
  ]
}

END METADATA -}

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Selectors as Sel
import Mation.Selectors ((#>>), (#<>))
import Mation.Core.Refs as Refs


type Model = Unit

initial :: Model
initial = unit

render :: forall m. Model -> E.Html m (M.Modify Model)
render _ =
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
        , S.border "1px solid gray"
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


main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }

