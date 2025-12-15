module Mation.Tests.SelectFlicker where

{- BEGIN METADATA

{
  "name": "Select Flicker",
  "desc": "Tests <select> elements flickering under certain conditions",
  "specs": [
    "You should testing on Firefox, not Chrome",
    "The <select> should not flicker when open"
  ]
}

END METADATA -}


{- Bug: select flickering

Under certain conditions, if a <select> is patched when it is open then it
will flicker and its hover state will be reset, even if it hasn't changed since the
previous frame.

- Sufficient conditions for this to occur are:
    - The <select> is not pruned
    - The <select> has a fixup prop that adds a class via either `.setAttribute('class', ...)`
      or via `.classList.add(...)`. The fixup prop's restoration function need not remove the
      class. (Corollary: the fixup need not be valid!)

- By "hover state" I mean the DOM-native hover state. In chrome, this is a blue
  background behind the hovered <option>

Cause: Potentially an FF issue? It repros on Firefox but not Chrome.

-}

import Mation.Core.Prelude

import Mation as M
import Mation (DomNode)
import Mation.Elems as E
import Mation.Props as P
import Mation.Props.Unsafe as P
import Mation.Lenses (field)
import Mation.Core.Refs as Refs


type SelectFlicker =
  { selected :: String
  , time :: Int
  }

initialSelectFlicker :: SelectFlicker
initialSelectFlicker =
  { selected: "A"
  , time: 0
  }

daemonSelectFlicker :: M.Daemon' SelectFlicker
daemonSelectFlicker ref = do
  _ <- everyNSeconds 0.75 do
    ref # Refs.modify (field @"time" %~ (_ + 1))
  pure unit

viewSelectFlicker :: SelectFlicker -> E.Html' (M.Modify SelectFlicker)
viewSelectFlicker model =
  E.div
  []
  [ E.p
    []
    [ E.text ("Time: " <> show model.time)
    ]
  , E.div
    []
    [ E.select
      [ P.fixup \node -> do
          node # addClass "princess"
          pure { restore: pure unit }
      , P.onInputValue \val -> M.modify (field @"selected" .~ val)
      ]
      [ ["A", "B", "C"] # foldMap \opt ->
        E.option
        [ P.value opt
        , P.selected (opt == model.selected)
        ]
        [ E.text ("Option " <> opt)
        ]
      ]
    ]
  ]

foreign import addClass :: String -> DomNode -> Effect Unit
foreign import everyNSeconds :: Number -> Effect Unit -> Effect { cancel :: Effect Unit }


main :: Effect Unit
main = do
  M.runApp
    { initial: initialSelectFlicker
    , render: viewSelectFlicker >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: daemonSelectFlicker
    }

