module Mation.Examples.Demo where

import Prelude
import Effect (Effect)

import Mation as M
import Mation.Elems as E
import Mation.Props as P

{- A simple counter example -}


type Model = Int


-- | Inital value for the model
initial :: Model
initial = 0


-- | How to display the model
render :: Model -> E.Html' Model
render model =
  E.div
  []
  [ E.p
    []
    [ E.text $ "The current counter value is: " <> show model
    ]
  , E.p
    []
    [ E.button
      [ P.onClick \_event ->
            M.mkPure  -- Pure update
              (_ + 1)
      ]
      [ E.text "Increment counter"
      ]
    ]
  ]

