{- SAMPLE_BEGIN -}
module Mation.Samples.Counter where
    
import Prelude
import Effect (Effect)
import Data.Foldable (fold)

import Mation as M
import Mation.Elems as E
import Mation.Props as P

-- Application state type
type Model = Int

-- Initial state value
initial :: Model
initial = 0

-- Displays the application and defines its behaviour
render :: Model -> E.Html' Model
render num = fold
  [ E.p
    []
    [ E.text $ "The current counter value is: " <> show num ]
  , E.p
    []
    [ E.button
      [ P.onClick        -- on click,
          \_ ->          -- ignore the click event
            \update ->   -- accept the state update function
              update     -- update the application state
                (_ + 1)  -- by incrementing the number
      ]
      [ E.text "Increment (x1)" ]
    , E.text " "
    , E.button
      [ P.onClick \_event update -> do
            -- An event handler is just an Effect
            -- Within one, you can do whatever you want!
            repeatedly { nTimes: 12, delaySeconds: 0.125 } do
              update (_ + 1)
      ]
      [ E.text "Increment (x12)" ]
    ]
  ]

foreign import repeatedly ::
  { nTimes :: Int, delaySeconds :: Number } -> Effect Unit -> Effect Unit

-- Run the app, mounting within <body>
main :: Effect Unit
main = M.runApp { initial, render, root: M.underBody, daemon: mempty }
{- SAMPLE_END -}

-- Note: changing `main` won't do anything because it's not actually used
-- This "application" is actually used as a component within a larger application; see AllSamples.purs
