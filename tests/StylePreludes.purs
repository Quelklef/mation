module Mation.Tests.StylePreludes where

{- BEGIN METADATA

{
  "name": "Style Preludes",
  "desc": "Tests style preludes",
  "specs": [
    "Both elements should have an animation"
  ]
}

END METADATA -}

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Core.Refs as Refs


type Model = Unit

initial :: Model
initial = unit

render :: forall m. Model -> E.Html m (M.Modify Model)
render _ =
  E.div
  [ P.addCss "display: flex; flex-direction: column; gap: 1em;"
  ]
  [ mkPreludeExample mempty
  , mkPreludeExample (S.fontWeight "bold")
  ]

  where

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


main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }

