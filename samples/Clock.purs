module Mation.Samples.Clock where

import Mation.Core.Prelude
import Data.Int (floor, toNumber)
import Data.Array (range)

import Mation as M
import Mation.Elems as E
import Mation.Styles as S
import Mation.Props as P


type Model =
  { hour :: Number
  , minute :: Number
  , second :: Number
  }

initial :: Model
initial = { hour: 0.0, minute: 0.0, second: 0.0 }

daemon :: M.Daemon' Model
daemon ref =
  watchTime \{ hour, minute, second } ->
    ref # M.write { hour, minute, second }

foreign import watchTime :: ({ hour :: Number, minute :: Number, second :: Number } -> Effect Unit) -> Effect Unit


render :: Model -> E.Html' Unit
render { hour, minute, second } =

  E.div
  [ P.addStyles
    [ S.display "flex"
    , S.flexDirection "column"
    , S.justifyContent "center"
    , S.alignItems "center"
    , S.gap "2em"
    , S.paddingTop "10vh"
    ]
  ]
  [ analog { hour, minute, second }
  , E.div
    [ P.addStyles
      [ S.fontFamily "monospace"
      , S.fontSize "18px"
      ]
    ]
    [ E.text $ let
        hour' = case floor hour of 0 -> 12
                                   n -> n
      in showXX hour' <> ":" <> showXX (floor minute) <> ":" <> showXX (floor second)
    ]
  ]

  where

  showXX :: Int -> String
  showXX = show >>> padStart 2 "0"


analog :: Model -> E.Html' Unit
analog { hour, minute, second } =

  E.div
  [ P.addStyles
    [ S.width (show size <> "px")
    , S.height (show size <> "px")
    , S.borderRadius "100%"
    , S.border "2px solid black"
    , S.position "relative"
    ]
  ]
  [ range 1 12 # foldMap \n -> hand (toNumber n / 12.0) 0.075 "right" "lightgrey"
  , hand (hour / 12.0) 0.5 "left" "black"
  , hand (minute / 60.0) 0.75 "left" "black"
  , hand (second / 60.0) 0.85 "left" "red"
  ]

  where

  size = 200.0

  hand n lengthModifier alignSelf color =
    E.div
    [ P.addStyles
      [ S.height "2px"
      , S.position "absolute"
      , S.width "50%"
      , S.top "calc(50% - 1px)"
      , S.left "50%"
      , S.transformOrigin "center left"
      , S.transform $ "rotate(" <> show (n - 0.25) <> "turn)"
      , S.display "inline-flex"
      , S.justifyContent $ if alignSelf == "left" then "flex-start" else "flex-end"
      , S.alignItems "stretch"
      ]
    ]
    [ E.span
      [ P.addStyles
        [ S.backgroundColor color
        , S.width $ show (size / 2.0 * lengthModifier) <> "px"
        ]
      ]
      []
    ]


foreign import padStart :: Int -> String -> String -> String

main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap (\_ -> unit)
    , root: M.underBody
    , daemon
    }

