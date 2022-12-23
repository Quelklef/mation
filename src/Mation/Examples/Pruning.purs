module Mation.Examples.Pruning where

import Mation.Core.Prelude

import Data.Lens.Lens.Tuple (_1, _2)
import Data.Array (range)

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Core.Util.UnsureEq (class UnsureEq)


type Double a = a /\ a

type Vals = Double (Double (Double (Double Int)))

type Model =
  { vals :: Vals
  , example2 :: { depth :: Int, color :: String }
  , sumTest :: Int /\ Int
  }

initial :: Model
initial =
  { vals: double (double (double (double 0)))
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
  [ E.prune "1" (\n -> box $ E.enroot _1 $ renderOne n) (model ^. _1)
  , E.prune "2" (\n -> box $ E.enroot _2 $ renderOne n) (model ^. _2)
  ]

  where

  box el =
    E.div
    [ P.style'
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
  [ P.style "font-family: sans-serif; font-size: 0.9em"
  ]
  [ E.p [ pSty ] [ E.text "Mation sports a feature called 'pruning' which allows you to avoid updating parts of the application when the relevant state doesn't change. Try incrementing the counters below. Updates are shown in red." ]
  , E.div
    [ P.style "font-size: 0.75em"
    ]
    [ flip (E.prune "example-1") model.vals (\vals -> E.enroot (prop (Proxy :: Proxy "vals")) $ onDouble (onDouble (onDouble (onDouble mkCounter))) vals)
    ]
  , E.br []
  , E.hr []
  , E.p [ pSty ] [ E.text "Pruning works even when the node moves around the application. Try modifying the number of wrapper <divs> below. Again, updates are shown in red." ]
  , flip (E.prune "example-2") model.example2 (\ex2 -> E.enroot (prop (Proxy :: Proxy "example2")) $ renderExample2 ex2)
  , E.br []
  , E.br []
  , E.hr []
  , E.enroot (prop (Proxy :: Proxy "sumTest")) $ E.prune "sum-test" renderSumTest model.sumTest
  ]

  where

  pSty = P.style' [ S.maxWidth "600px" ]

  mkCounter :: Int -> E.Html' Int
  mkCounter = \val ->
    E.span
    [ P.style'
      [ S.display "inline-flex"
      , S.padding ".1em"
      , S.alignItems "center"
      ]
    ]
    [ E.span
      [ P.style "margin-right: 5px" ]
      [ E.text (show val) ]
    , E.span
      [ P.style'
        [ S.display "inline-flex"
        , S.flexDirection "column"
        ]
      ]
      [ E.button
        [ P.onClick \_ -> M.mkPure (_ + 1)
        , P.style "font-size: 1em; padding: .1em .2em"
        ]
        [ E.text "↑" ]
      , E.button
        [ P.onClick \_ -> M.mkPure (_ - 1)
        , P.style "font-size: 1em; padding: .1em .2em"
        ]
        [ E.text "↓" ]
      ]
    ]


renderExample2 :: { depth :: Int, color :: String } -> E.Html' { depth :: Int, color :: String }
renderExample2 { depth, color } =

  fold
  [ E.div
    []
    [ E.text "Depth: "
    , E.input
      [ P.type_ "number"
      , P.value $ show depth
      , P.onInput' $ parseInt >>> (prop (Proxy :: Proxy "depth") .~ _) >>> M.mkPure
      ]
    , E.text " "
    , E.button
      [ P.onClick \_ -> M.mkPure $ prop (Proxy :: Proxy "color") %~ case _ of
            "lightgreen" -> "teal"
            "teal" -> "lightgreen"
            _ -> "yellow"
      ]
      [ E.text "swap colors"
      ]
    ]
  , E.br []
  , doIt depth
  ]

  where

  doIt depth =
    if depth == 0
    then
      E.div
      [ P.style'
        [ S.display "flex"
        , S.gap "2px"
        , S.maxWidth "450px"
        , S.flexWrap "wrap"
        , S.justifyContent "center"
        , S.padding "1em"
        ]
      ]
      [ (_ `power` 200) $
        E.div
        [ P.style'
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
      [ P.style'
        [ S.display "inline-block"
        , S.border "1px solid lightgrey"
        , S.padding ".4em"
        , S.margin "0"
        ]
      , P.showUpdates
      ]
      [ doIt (depth - 1)
      ]

foreign import parseInt :: String -> Int


-- Tests cases where we cycle between different pruned VDOMs
renderSumTest :: (Int /\ Int) -> E.Html' (Int /\ Int)
renderSumTest (n /\ tab) =
  E.div
  []
  [ E.p
    []
    [ E.button
      [ P.onClick \_ -> M.mkPure (_1 %~ (_ + 1))
      , P.style "padding: 0 1.5em; line-height: 2em"
      ]
      [ E.text (show n)
      ]
    , E.text " "
    , E.button
      [ P.onClick \_ -> M.mkPure (_2 %~ (_ + 1))
      , P.style "padding: 0 1.5em; line-height: 2em"
      ]
      [ E.text "swap"
      ]
    , E.span [ P.style "padding: 0 1em" ] [ ]
    , case tab `mod` 3 of
        0 -> flip (E.prune "0") n (\n -> E.span [ P.style "border: 1px solid blue" ] [ E.text (show n <> " ") ])
        1 -> flip (E.prune "1") n (\n -> range 1 n # foldMap \k -> E.text (show k <> " .. "))
        _ -> flip (E.prune "_") n (\n -> E.span [ P.style "background-color: red; color: white" ] [ E.text "three" ])
    ]
  ]
