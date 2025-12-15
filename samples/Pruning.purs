module Mation.Samples.Pruning where

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Lenses (field)
import Mation.Core.Refs as Refs


type Double a = a /\ a

type Vals = Double (Double (Double Int))

type Model =
  { vals :: Vals
  , example2 :: { depth :: Int, color :: String }
  }

initial :: Model
initial =
  { vals: double (double (double 0))
  , example2: { depth: 4, color: "teal" }
  }

  where

  double :: forall a. a -> Double a
  double x = x /\ x


onDouble :: forall ref a. M.FocusRefWithLens ref => Eq a =>
  (a -> E.Html' (ref a)) -> (Double a -> E.Html' (ref (Double a)))
onDouble renderOne model =
  E.div
  []
  [ (model ^. _1) # E.pruneEq "1" (\n -> box $ cmap (M.focusWithLens _1) $ renderOne n)
  , (model ^. _2) # E.pruneEq "2" (\n -> box $ cmap (M.focusWithLens _2) $ renderOne n)
  ]

  where

  box el =
    E.div
    [ P.addStyles
      [ S.display "inline-block"
      , S.border "1px solid lightgrey"
      , S.padding ".2em"
      , S.margin ".2em"
      ]
    , P.showUpdates
    ]
    [ el
    ]


render :: Model -> E.Html' (M.Modify Model)
render model =

  E.div
  [ P.addCss "font-family: sans-serif"
  ]
  [ E.p [ pSty ] [ E.text "Mation sports a feature called 'pruning' which allows you to avoid updating parts of the application when the relevant state doesn't change. Try incrementing the counters below. Updates are shown in red." ]
  , E.div
    [ P.addCss "font-size: 0.75em"
    ]
    [ model.vals # E.pruneEq "example-1" (\vals -> cmap (M.focusWithLens (field @"vals")) $ onDouble (onDouble (onDouble mkCounter)) vals)
    ]
  , E.br []
  , E.hr []
  , E.p [ pSty ] [ E.text "Pruning works even when the node moves around the application. Try modifying the number of wrapper <divs> below. Again, updates are shown in red." ]
  , model.example2 # E.pruneEq "example-2" (\ex2 -> cmap (M.focusWithLens (field @"example2")) $ renderExample2 ex2)
  ]

  where

  pSty = P.addStyles [ S.maxWidth "600px" ]

  mkCounter :: Int -> E.Html' (M.Modify Int)
  mkCounter = \val ->
    E.span
    [ P.addStyles
      [ S.display "inline-flex"
      , S.padding ".1em"
      , S.alignItems "center"
      ]
    ]
    [ E.span
      [ P.addCss "margin-right: 5px" ]
      [ E.text (show val) ]
    , E.span
      [ P.addStyles
        [ S.display "inline-flex"
        , S.flexDirection "column"
        ]
      ]
      [ E.button
        [ P.onClick \_ -> M.modify (_ + 1)
        , P.addCss "font-size: 1em; padding: .1em .2em"
        ]
        [ E.text "↑" ]
      , E.button
        [ P.onClick \_ -> M.modify (_ - 1)
        , P.addCss "font-size: 1em; padding: .1em .2em"
        ]
        [ E.text "↓" ]
      ]
    ]


renderExample2 :: { depth :: Int, color :: String } -> E.Html' (M.Modify { depth :: Int, color :: String })
renderExample2 = \{ depth, color } ->

  fold
  [ E.div
    []
    [ E.text "Depth: "
    , E.input
      [ P.type_ "number"
      , P.value $ show depth
      , P.onInputValue \v -> M.modify (field @"depth" .~ parseInt v)
      ]
    , E.text " "
    , E.button
      [ P.onClick \_ -> M.modify $ field @"color" %~ case _ of
            "lightgreen" -> "teal"
            "teal" -> "lightgreen"
            _ -> "yellow"
      ]
      [ E.text "swap colors"
      ]
    ]
  , E.br []
  , doIt depth color
  ]

  where

  doIt depth color =
    if depth <= 0
    then
      E.div
      [ P.addStyles
        [ S.display "flex"
        , S.gap "2px"
        , S.maxWidth "450px"
        , S.flexWrap "wrap"
        , S.justifyContent "center"
        , S.padding "1em"
        ]
      ]
      [ color # E.pruneEq "the boxes" \color ->  (_ `power` 60) $
        E.div
        [ P.addStyles
          [ S.display "inline-block"
          , S.minWidth "20px"
          , S.minHeight "20px"
          , S.borderRadius "3px"
          , S.backgroundColor color
          ]
        , P.showUpdates
        ]
        []
      ]
    else
      E.div
      [ P.addStyles
        [ S.display "inline-block"
        , S.border "1px solid lightgrey"
        , S.padding ".4em"
        , S.margin "0"
        ]
      , P.showUpdates
      ]
      [ doIt (depth - 1) color
      ]

foreign import parseInt :: String -> Int


main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }
