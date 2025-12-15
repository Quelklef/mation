module Mation.Tests.WithK where

{- BEGIN METADATA

{
  "name": "withK",
  "desc": "Tests Mation.Elems.withK",
  "specs": [
    "Clicking either button should increment its counter"
  ]
}

END METADATA -}

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Core.Refs as Refs
import Mation.Lenses (field)


type WithKTest =
  { a :: Int
  , b :: Int
  }

viewWithKTest :: WithKTest -> E.Html' (M.Modify WithKTest)
viewWithKTest { a, b } =
  E.div
  []
  [ E.p
    []
    [ E.button
      [ P.onClick \_ -> M.modify (field @"a" %~ (_ + 1))
      ]
      [ E.text ("A: " <> show a)
      ]
    ]
  , E.text " "
  , E.withK \ref ->
    E.p
    []
    [ E.button
      [ P.onClick \_ _ -> ref # M.modify (field @"b" %~ (_ + 1))
      ]
      [ E.text ("B: " <> show b)
      ]
    ]
  ]

main :: Effect Unit
main = do
  M.runApp
    { initial: { a: 0, b: 0 }
    , render: viewWithKTest >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }

