module Mation.Tests.Censor where

{- START METADATA

{
  "name": "Censor",
  "desc": "Tests Mation.Core.Elems.censor",
  "specs": [
    "The button should increment the counter IFF \"censor\" is disabled"
  ]
}

END METADATA -}

import Mation.Core.Prelude

import Mation as M
import Mation.Lenses (field)
import Mation (Daemon', DomNode)
import Mation.Elems as E
import Mation.Props as P
import Mation.Core.Refs as Refs


type Model =
  { shouldCensor :: Boolean
  , counter :: Int
  }

view :: Model -> M.Html' (M.Modify Model)
view model =
  E.div
  []
  [ E.p
    []
    [ E.button
      [ P.onClick \_ -> M.modify (field @"counter" %~ add 1) ]
      [ E.text "Increment counter" ]
      # (if model.shouldCensor then E.censor else identity)
    , E.text " "
    , E.text $ show $ model.counter
    ]
  , E.p
    []
    [ E.input
      [ P.type_ "checkbox"
      , P.id "do-censor"
      , P.value $ show model.shouldCensor
      , P.onClick \_ -> Refs.modify (field @"shouldCensor" %~ not)
      ]
    , E.label
      [ P.for "do-censor"
      ]
      [ E.text " "
      , E.text "Censor"
      ]
    ]
  ]

main :: Effect Unit
main = do
  M.runApp
    { initial: { shouldCensor: false, counter: 0 }
    , render: view >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }

