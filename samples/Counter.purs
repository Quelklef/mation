{- SAMPLE_BEGIN -}
module Mation.Samples.Counter where

import Prelude
import Effect (Effect)
import Data.Foldable (fold)
import Data.Functor.Contravariant (cmap)

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Core.Refs as Refs

-- Application state type
type Model = Int

-- Initial state value
initial :: Model
initial = 0

-- Displays the application and defines its behaviour
render :: Model -> E.Html' (M.Modify Model)
render num = fold
  [ E.p
    []
    [ E.text $ "The current counter value is: " <> show num ]
  , E.p
    []
    [ E.button
      [ P.onClick      -- on click,
          \_ ->        -- ignore the click event
            M.modify   -- update the model
              (_ + 1)  -- by adding one
      ]
      [ E.text "Increment (x1)" ]
    , E.text " "
    , E.button
      [ P.onClick \_event modelRef -> do
          -- An event handler is just an Effect
          -- Within one, you can do whatever you want!
          repeatedly { nTimes: 12, delaySeconds: 0.125 } do
            modelRef # M.modify (_ + 1)
      ]
      [ E.text "Increment (x12)" ]
    ]
  ]

foreign import repeatedly ::
  { nTimes :: Int, delaySeconds :: Number } -> Effect Unit -> Effect Unit

-- Run the app, mounting within <body>
main :: Effect Unit
main = M.runApp
  { initial
  , render: render >>> cmap Refs.downcast
  , root: M.underBody
  , daemon: mempty
  }
{- SAMPLE_END -}

-- Note: changing `main` won't do anything because it's not actually used
-- This "application" is actually used as a component within a larger application; see AllSamples.purs
