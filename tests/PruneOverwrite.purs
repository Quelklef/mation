module Mation.Tests.PruneOverwrite where

{- BEGIN METADATA

{
  "name": "Prune Overwrite",
  "desc": "Tests a diffing bug",
  "specs": [
    "Clicking the button should cycle between (A) showing \"non-pruned\" and \"pruned\", and (B) showing only \"pruned\""
  ]
}

END METADATA -}


{-

BUG: If a non-pruned <span> is rendered where a pruned element
just was, then diffing of the pruned element will break.

The issue appears to be that the non-pruned <span> is wrongly
diffing against the rendered <span style="display: contents"> of
the pruned node.

-}

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Lenses (field)
import Mation.Core.Refs as Refs


type Model =
  { showSpan :: Boolean
  }

view :: Model -> E.Html' (M.Modify Model)
view model =
  E.div
  [ P.addCss "font-family: sans-serif"
  ]
  [ E.p []
    [ E.button
      [ P.onClick \_ -> Refs.modify (field @"showSpan" %~ not) ]
      [ E.text "Click me" ]
    ]

  , guard model.showSpan $ fold
    [ E.span [] [ E.text "non-pruned" ]
    , E.br [] `power` 2
    ]
  , flip (E.pruneEq "the-prune") unit \_ -> E.div [] [ E.text "pruned" ]
  ]

main :: Effect Unit
main = do
  M.runApp
    { initial: { showSpan: true }
    , render: view >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }

