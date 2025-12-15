module Mation.Tests.PhoneNumber where

{- BEGIN METADATA

{
  "name": "Phone Number",
  "desc": "Tests shared state and <select> flickering",
  "specs": [
    "The two phone number inputs should share state",
    "No <select> should flicker when open"
  ]
}

END METADATA -}

import Mation.Core.Prelude

import Data.Array (range)

import Mation as M
import Mation (ReadWriteL, Modify)
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Core.Refs (focusWithLens)
import Mation.Core.Refs as Refs
import Mation.Lenses (field)


type PhoneNumber = Int /\ Int /\ Int /\ Int /\ Int /\ Int /\ Int /\ Int /\ Int /\ Int

renderPhoneNumber :: PhoneNumber -> E.Html' (M.Modify PhoneNumber)
renderPhoneNumber = \pn ->
  E.div
  [ P.remark "phone number input"
  , P.addStyles
    [ S.display "flex"
    , S.gap "5px"
    , S.alignItems "center"
    , S.margin "1em 0"
    ]
  ]
  [ digit _1 pn
  , digit (_2 <<< _1) pn
  , digit (_2 <<< _2 <<< _1) pn
  , dot
  , digit (_2 <<< _2 <<< _2 <<< _1) pn
  , digit (_2 <<< _2 <<< _2 <<< _2 <<< _1) pn
  , digit (_2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _1) pn
  , dot
  , digit (_2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _1) pn
  , digit (_2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _1) pn
  , digit (_2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _1) pn
  , digit (_2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _2 <<< _2) pn
  ]

  where

  dot = E.span [ ] [ E.rawHtml "&bull;" ]

  digit :: Lens' PhoneNumber Int -> PhoneNumber -> E.Html' (M.Modify PhoneNumber)
  digit len pn =
    E.select
    [ P.onInputValue \v -> M.modify (len .~ (parseInt v))
    , P.remark "phone number digit"
    ]
    [ flip foldMap (range 1 9) \n ->
        E.option
        [ if n == pn ^. len then P.selected true else mempty
        -- P.selected $ n == pn ^. len   -- same thing
        ]
        [ E.text (show n) ]
    ]

foreign import parseInt :: String -> Int


type Model =
  { phoneNumber :: PhoneNumber
  , counter :: Int
  }

initial :: Model
initial =
  { phoneNumber: 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0
  , counter: 0
  }

render :: Model -> E.Html' (Modify Model)
render model =
  E.div
  []
  [ renderPhoneNumber model.phoneNumber
      # cmap (focusWithLens (field @"phoneNumber"))
  , renderPhoneNumber model.phoneNumber
      # cmap (focusWithLens (field @"phoneNumber"))
  , E.text $ "Counter: " <> show model.counter
  ]

daemon :: ReadWriteL Model -> Effect Unit
daemon ref = do
  everyNSeconds 0.5 do
    ref # Refs.modify (field @"counter" %~ add 1)

foreign import everyNSeconds :: Number -> Effect Unit -> Effect Unit


main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon
    }

