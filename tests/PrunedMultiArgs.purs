module Mation.Tests.PrunedMultiArgs where

{- METADATA START

{
  "name": "Pruned Multi-Args",
  "desc": "Tests pruneEq on a multi-argument function",
  "specs": [
    "Clicking each of A++, B++, C++ should increment their respective counters",
    "Clicking \"Noop\" should not cause an update"
  ]
}

METADATA END -}

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Lenses (field)
import Mation.Core.Refs as Refs


type Model =
  { a :: Int
  , b :: Int
  , c :: Int
  }

initial :: Model
initial = { a: 0, b: 0, c: 0 }

render :: Model -> E.Html' (M.Modify Model)
render model =
  E.pruneEq "multi-arg-test"
  renderImpl
  model.a model.b model.c

renderImpl :: Int -> Int -> Int -> E.Html' (M.Modify Model)
renderImpl a b c =
  E.div
  [ P.showUpdates
  ]
  [ E.text $ "A=" <> show a
  , E.text "; "
  , E.text $ "B=" <> show b
  , E.text "; "
  , E.text $ "C=" <> show c
  , E.text "; "
  , E.button
    [ P.onClick \_ ref -> ref # M.modify (field @"a" %~ (_ + 1)) ]
    [ E.text "A++" ]
  , E.text " "
  , E.button
    [ P.onClick \_ ref -> ref # M.modify (field @"b" %~ (_ + 1)) ]
    [ E.text "B++" ]
  , E.text " "
  , E.button
    [ P.onClick \_ ref -> ref # M.modify (field @"c" %~ (_ + 1)) ]
    [ E.text "C++" ]
  , E.text " "
  , E.button
    [ P.onClick \_ ref -> ref # M.modify identity ]
    [ E.text "Noop" ]
  ]


main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }
