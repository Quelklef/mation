module Mation.Samples.Pruning where

import Mation.Core.Prelude

import Data.Array (range)

import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Core.Util.UnsureEq (class UnsureEq)
import Mation.Lenses (field)


type Double a = a /\ a

type Vals = Double (Double (Double Int))

type Model =
  { vals :: Vals
  , example2 :: { depth :: Int, color :: String }
  , sumTest :: Int /\ Int
  }

initial :: Model
initial =
  { vals: double (double (double 0))
  , example2: { depth: 4, color: "teal" }
  , sumTest: 0 /\ 0
  }

  where

  double :: forall a. a -> Double a
  double x = x /\ x


onDouble :: forall a. UnsureEq a => (a -> E.Html' a) -> (Double a -> E.Html' (Double a))
onDouble renderOne model =
  E.div
  []
  [ E.prune "1" (model ^. _1) (\n -> box $ E.enroot _1 $ renderOne n)
  , E.prune "2" (model ^. _2) (\n -> box $ E.enroot _2 $ renderOne n)
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


render :: Model -> E.Html' Model
render model =

  E.div
  [ P.addCss "font-family: sans-serif; font-size: 0.9em"
  ]
  [ E.p [ pSty ] [ E.text "Mation sports a feature called 'pruning' which allows you to avoid updating parts of the application when the relevant state doesn't change. Try incrementing the counters below. Updates are shown in red." ]
  , E.div
    [ P.addCss "font-size: 0.75em"
    ]
    [ E.prune "example-1" model.vals (\vals -> E.enroot (field @"vals") $ onDouble (onDouble (onDouble mkCounter)) vals)
    ]
  , E.br []
  , E.hr []
  , E.p [ pSty ] [ E.text "Pruning works even when the node moves around the application. Try modifying the number of wrapper <divs> below. Again, updates are shown in red." ]
  , E.prune "example-2" model.example2 (\ex2 -> E.enroot (field @"example2") $ renderExample2 ex2)
  , E.br []
  , E.br []
  , E.hr []
  , E.enroot (field @"sumTest") $ E.prune "sum-test" model.sumTest renderSumTest
  ]

  where

  pSty = P.addStyles [ S.maxWidth "600px" ]

  mkCounter :: Int -> E.Html' Int
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
        [ P.onClick \_ step -> step (_ + 1)
        , P.addCss "font-size: 1em; padding: .1em .2em"
        ]
        [ E.text "↑" ]
      , E.button
        [ P.onClick \_ step -> step (_ - 1)
        , P.addCss "font-size: 1em; padding: .1em .2em"
        ]
        [ E.text "↓" ]
      ]
    ]


renderExample2 :: { depth :: Int, color :: String } -> E.Html' { depth :: Int, color :: String }
renderExample2 = \{ depth, color } ->

  fold
  [ E.div
    []
    [ E.text "Depth: "
    , E.input
      [ P.type_ "number"
      , P.value $ show depth
      , P.onInputValue \v step -> step (field @"depth" .~ parseInt v)
      ]
    , E.text " "
    , E.button
      [ P.onClick \_ step -> step $ field @"color" %~ case _ of
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
      [ E.prune "the boxes" color \color ->  (_ `power` 60) $
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


-- Tests cases where we cycle between different pruned VDOMs
renderSumTest :: (Int /\ Int) -> E.Html' (Int /\ Int)
renderSumTest (n /\ tab) =
  E.p
  []
  [ E.div
    [ P.addStyles
      [ S.border "1px solid black"
      , S.borderBottom "none !important"
      , S.padding "1em 2em"
      ]
    ]
    [ E.text "Number: "
    , E.button
      [ P.onClick \_ step -> step (_1 %~ (_ + 1))
      , P.addStyles
        [ S.padding "0 1.5em"
        , S.lineHeight "2em"
        , S.cursor "pointer"
        ]
      ]
      [ E.text (show n)
      ]
    , E.text "; "
    , E.text "Tab: "
    , mkTab 0 "one"
    , E.text " "
    , mkTab 1 "two"
    , E.text " "
    , mkTab 2 "three"
    ]
  , E.div
    [ P.addStyles
      [ S.border "1px solid black"
      , S.padding "1em 2em"
      ]
    ]
    [ case tab `mod` 3 of
        0 -> E.prune "0" n (\n -> E.span [ P.addCss "border: 1px solid blue" ] [ E.text (show n <> " ") ])
        1 -> E.prune "1" n (\n -> range 1 n # foldMap \k -> E.text (show k <> " .. "))
        _ -> E.prune "_" n (\_ -> E.span [ P.addCss "background-color: red; color: white" ] [ E.text "three" ])
    ]
  ]

  where

  mkTab idx label =
    E.span
    [ P.onClick \_ step -> step (_2 .~ idx)
    , P.addStyles
      [ S.cursor "pointer"
      , S.padding ".35em 1em"
      , S.display "inline-block"
      , S.border "1px solid black"
      ]
    , guard (tab == idx) $
        P.addStyles
          [ S.textDecoration "underline"
          ]
    ]
    [ E.text label
    ]

