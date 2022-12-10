module Mation.Examples.Styling where

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Core.Style as CS
import Mation.Core.Util.PuncturedFold (PuncturedFold (PF), Elem (..))
import Mation.Core.Util.FreeMonoid as FM

type Model = Unit

initial :: Model
initial = unit

render :: forall m. Model -> E.Html m Model
render model =
  E.div
  [ P.style "display: flex; flex-direction: column; gap: 1em;"
  ]
  [ mkExample
      "I style on hover"
      S.onHover

  , mkExample
      "I style when the page is ≤500px tall"
      (\styles -> FM.singleton $ CS.SScopeABlock "@media (max-height: 500px)" $ CS.SConcat $ FM.float $ styles)

  , mkExample
      "I style my children"
      S.onChildren

  , mkExample
      "I style my children when they are hovered"
      (S.onChildren >>> singleton >>> S.onHover)
        -- FIXME: this one^ is broken and I think it's due to a hash collision

  , mkExample
      "I style my children when I am hovered"
      (S.onHover >>> singleton >>> S.onChildren)
      
  ]

  where

  singleton x = [x]

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
