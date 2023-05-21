module Mation.Examples.Welcome where

import Mation.Core.Prelude

import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S


type Model = Unit

initialize :: Effect Model
initialize = pure unit

render :: Model -> E.Html' Model
render _model =
  E.div
  [ P.style'
    [ S.maxWidth "600px"
    , S.fontFamily "sans-serif"
    ]
  ]
  [ E.h2 [] [ E.text "Mation live demo" ]
  , E.p
    []
    [ E.text "Hello!" ]
  , E.ul
    []
    [ E.li [ sLi ] [ E.text "Home of examples (and test cases)! Use the nav at the top." ]
    , E.li
      [ sLi ]
      [ fold
        [ E.text "Edit these examples! All code is located at "
        , E.code [ sCode ] [ E.text "${the_demo_directory}/src/Mation/Examples" ]
        , E.text "."
        ]
      ]
    ]
  ]

  where

  sLi = P.style'
    [ S.margin ".5em 0"
    ]

  sCode = P.style'
    [ S.fontSize "1em"
    , S.backgroundColor "rgb(230, 230, 230)"
    , S.padding "2px 4px"
    , S.borderRadius "4px"
    ]
