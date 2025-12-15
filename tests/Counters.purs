module Mation.Tests.Counters where

{- BEGIN METADATA

{
  "name": "Counters",
  "desc": "Basic test of rendering, styles, IO, async, and components",
  "specs": [
    "+ and - should increment/decrement their respective counters",
    "++ and -- should begin a streaming increment/decrement of their respective counters",
    "🛑 should stop its respective in-progress stream"
  ]
}

END METADATA -}

import Mation.Core.Prelude

import Data.Array (range)
import Data.Map as Map

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Selectors as Sel
import Mation.Selectors ((#<>))
import Mation.Core.Refs as Refs
import Mation.Core.Util.UnsureEq (class UnsureEq, genericUnsureEq)
import Mation.Lenses (field)


type Counter = { count :: Int, streamState :: StreamState }

data StreamState = NotStreaming | Streaming { cancel :: Effect Unit }

derive instance Generic StreamState _

instance UnsureEq StreamState where
  unsureEq = genericUnsureEq

renderCounter :: Counter -> E.Html' (M.Modify Counter)
renderCounter model =
  E.div
  [ P.addStyles
    [ S.fontSize "1.5em"
    , S.on (Sel.this #<> Sel.descendantsWhere "button")
      [ S.fontFamily "monospace"
      ]
    , S.on (Sel.descendantsWhere "button")
      [ S.width "6ch"
      , S.textAlign "center"
      , S.fontSize "inherit"
      ]
    ]
  ]
  [ E.span
    [ P.addStyles
      [ S.display "inline-block"
      , S.minWidth "10ch"
      ]
    , P.addDataset $
        if model.count < 1 then mempty
        else range 1 (min model.count 50) # foldMap (\n -> [show n /\ show n]) # Map.fromFoldable
    ]
    [ E.text (show model.count)
    ]
  , E.text " | "
  , E.button
    [ P.onClick \_ -> M.modify (_count %~ (_ + 1))
    , buttonStyle
    ]
    [ E.text "+"
    ]
  , E.text " "
  , E.button
    [ P.onClick \_ -> M.modify (_count %~ (_ - 1))
    , buttonStyle
    ]
    [ E.text "-"
    ]
  , E.text " "
  , case model.streamState of
      NotStreaming -> fold
        [ E.button
          [ P.onClick \_ ref -> do
              { cancel } <- everyNSeconds 0.05 (ref # M.modify (_count %~ (_ + 1)))
              ref # M.modify (_streamState .~ Streaming { cancel })
          , buttonStyle
          ]
          [ E.text "++"
          ]
        , E.text " "
        , E.button
          [ P.onClick \_ ref -> do
              { cancel } <- everyNSeconds 0.05 (ref # M.modify (_count %~ (_ - 1)))
              ref # M.modify (_streamState .~ Streaming { cancel })
          , buttonStyle
          ]
          [ E.text "--"
          ]
        ]
      Streaming { cancel } ->
        E.button
        [ P.onClick \_ ref -> do cancel *> (ref # M.modify (_streamState .~ NotStreaming))
        , buttonStyle
        ]
        [ E.text "🛑"
        ]
  ]

  where

  _count = field @"count"
  _streamState = field @"streamState"

  buttonStyle :: forall m s. P.Prop m s
  buttonStyle = P.addStyles
    [ S.borderRadius "0"
    , S.borderColor "red"
    ]

foreign import everyNSeconds :: Number -> Effect Unit -> Effect { cancel :: Effect Unit }



type Model =
  { counter1 :: Counter
  , counter2 :: Counter
  }

initialize :: Effect Model
initialize = do
  pure
    { counter1: { count: 0, streamState: NotStreaming }
    , counter2: { count: 0, streamState: NotStreaming }
    }

render :: Model -> E.Html' (M.Modify Model)
render model =
  E.div
  []
  [ E.p [] [ cmap (M.focusWithLens (field @"counter1")) (renderCounter model.counter1) ]
  , E.p [] [ cmap (M.focusWithLens (field @"counter2")) (renderCounter model.counter2) ]
  ]

main :: Effect Unit
main = do
  initial <- initialize
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }

