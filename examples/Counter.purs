
{- | Counter app -}

module Mation.Examples.Counter where
  
import Prelude
import Effect (Effect)
import Data.Foldable (fold)

import Mation as M
import Mation.Elems as E
import Mation.Props as P


-- | Application state
type Model = Int


-- | Application state initial value
initial :: Model
initial = 0


-- | How to display the application
render :: Model -> E.Html' Model
render num =
  fold
  [ E.p
    []
    [ E.text $ "The current counter value is: " <> show num ]
  , E.p
    []
    [ E.button
      [ P.onClick      -- on click,
          \_event ->   -- ignore the incoming event
            M.mkPure   -- perform a pure state update
              (_ + 1)  -- which increments the number
      ]
      [ E.text "Increment counter" ]
    ]
  ]


-- | Run the app, mounting within <body>
main :: Effect Unit
main = M.runApp' { initial, render, root: M.underBody }
