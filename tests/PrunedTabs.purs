module Mation.Tests.PrunedTabs where

{- METADATA START

{
  "name": "Pruned Tabs",
  "desc": "Tests cycling between different pruned VDOMs",
  "specs": [
    "Cycling between the tabs should work",
    "Clicking the button should increment the number and affect tab content"
  ]
}

METADATA END -}

import Mation.Core.Prelude

import Data.Array (range)

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Core.Refs as Refs


type Model = Int /\ Int

initial :: Model
initial = 0 /\ 0

-- Tests cases where we cycle between different pruned VDOMs
render :: Model -> E.Html' (M.Modify (Int /\ Int))
render (n /\ tab) =
  E.div
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


main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }
