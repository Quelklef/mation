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
import Mation.Core.Refs as Refs
import Mation.Lenses (field)


type Model =
  { number :: Int  -- Always ≥1
  , tabIdx :: Int
  }

initial :: Model
initial =
  { number: 1
  , tabIdx: 0
  }

render :: Model -> E.Html' (M.Modify Model)
render model =
  E.div
  [ P.addCss "font-family: sans-serif"
  ]
  [ E.p
    []
    [ E.text "Tab: "
    , mkTab 0 "one"
    , E.text " "
    , mkTab 1 "two"
    , E.text " "
    , mkTab 2 "three"
    ]
  , E.p
    []
    [ E.text $ "Number: " <> show model.number
    , E.text " "
    , E.button
      [ P.onClick \_ -> M.modify (field @"number" %~ add one)
      , P.addCss "cursor: pointer; font-family: monospace"
      ]
      [ E.text "++" ]
    , E.text " "
    , E.button
      [ P.onClick \_ -> M.modify (field @"number" %~ (add (-1) >>> max one))
      , P.addCss "cursor: pointer; font-family: monospace"
      ]
      [ E.text "--" ]
    ]
  , E.p
    []
    [ case model.tabIdx `mod` 3 of
      0 -> model.number # E.pruneEq "0" (\n -> E.div [] [ E.text (show n <> " ") ])
      1 -> model.number # E.pruneEq "1" (\n -> range 1 n # foldMap \k -> E.text (show k <> "... "))
      _ -> model.number # E.pruneEq "_" (\_ -> E.div [] [ E.text "meow" ])
    ]
  ]

  where

  mkTab idx label =
    E.span
    [ P.onClick \_ -> M.modify (field @"tabIdx" .~ idx)
    , P.addCss "cursor: pointer; display: inline-block; padding: 0.25em 0.5em"
    , guard (model.tabIdx == idx) $ P.addCss "font-weight: bold"
    ]
    [ E.text $ "[" <> label <> "]" ]


main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }
