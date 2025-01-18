module Mation.Samples.Pruning where

import Mation.Core.Prelude

import Data.Array (range)

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Lenses (field)


type Double a = a /\ a

type Vals = Double (Double (Double Int))

type Model =
  { vals :: Vals
  , example2 :: { depth :: Int, color :: String }
  , sumTest :: Int /\ Int
  , multiArgTest ::
      { a :: Int
      , b :: Int
      }
  }

initial :: Model
initial =
  { vals: double (double (double 0))
  , example2: { depth: 4, color: "teal" }
  , sumTest: 0 /\ 0
  , multiArgTest: { a: 0, b: 0 }
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
  [ P.addCss "font-family: sans-serif; font-size: 0.9em"
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
  , E.br []
  , E.br []
  , E.hr []
  , cmap (M.focusWithLens (field @"sumTest")) $ model.sumTest # E.pruneEq "sum-test" renderSumTest
  , E.br []
  , E.br []
  , E.hr []
  , multiArgTest model.multiArgTest.a model.multiArgTest.b
    # cmap (M.focusWithLens (prop (Proxy :: Proxy "multiArgTest")))
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


-- Tests cases where we cycle between different pruned VDOMs
renderSumTest :: (Int /\ Int) -> E.Html' (M.Modify (Int /\ Int))
renderSumTest (n /\ tab) =
  E.div
  []
  [ E.p [ P.addCss "font-weight: bold" ] [ E.text "Tab test" ]
  , E.div
    [ P.addStyles
      [ S.border "1px solid black"
      , S.borderBottom "none !important"
      , S.padding "1em 2em"
      ]
    ]
    [ E.text "Number: "
    , E.button
      [ P.onClick \_ -> M.modify (_1 %~ (_ + 1))
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
        0 -> n # E.pruneEq "0" (\n -> E.span [ P.addCss "border: 1px solid blue" ] [ E.text (show n <> " ") ])
        1 -> n # E.pruneEq "1" (\n -> range 1 n # foldMap \k -> E.text (show k <> " .. "))
        _ -> n # E.pruneEq "_" (\_ -> E.span [ P.addCss "background-color: red; color: white" ] [ E.text "three" ])
    ]
  ]

  where

  mkTab idx label =
    E.span
    [ P.onClick \_ -> M.modify (_2 .~ idx)
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


multiArgTest :: Int -> Int -> E.Html' (M.Modify { a :: Int, b :: Int })
multiArgTest =
  E.pruneEq "multi-arg-test"
  \a b ->
    E.div
    []
    [ E.span [ P.addCss "font-weight: bold" ] [ E.text "Multi-arg test: " ]
    , E.text ("A=" <> show a)
    , E.text "; "
    , E.text ("B=" <> show b)
    , E.text "; "
    , E.button
      [ P.onClick \_ ref -> ref # M.modify (prop (Proxy :: Proxy "a") %~ (_ + 1))
      ]
      [ E.text "+A"
      ]
    , E.text " "
    , E.button
      [ P.onClick \_ ref -> ref # M.modify (prop (Proxy :: Proxy "b") %~ (_ + 1))
      ]
      [ E.text "+B"
      ]
    ]
