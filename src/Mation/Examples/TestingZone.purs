module Mation.Examples.TestingZone where

import Mation.Core.Prelude

import Data.Array (range)

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Core.Util.UnsureEq (class UnsureEq, Unsure (..), viaPrim)

foreign import repeatedly :: Effect Unit -> Effect { cancel :: Effect Unit }

type Counter = { count :: Int, streamState :: StreamState }

data StreamState = NotStreaming | Streaming { cancel :: Effect Unit }

-- FIXME: genericUnsureEq not working when the type contains a record?
instance UnsureEq StreamState where
  unsureEq NotStreaming NotStreaming = Surely true
  unsureEq (Streaming c) (Streaming c') = viaPrim c c'
  unsureEq _ _ = Surely false

renderCounter :: Counter -> E.Html' Counter
renderCounter model =
  E.div
  [ ]
  [ E.text (show model.count)
  , E.text " "
  , E.button
    [ P.onClick \_ -> M.mkPure (_count %~ (_ + 1))
    , buttonStyle
    ]
    [ E.text "increment once"
    ]
  , E.text " "
  , case model.streamState of
      NotStreaming ->
        E.button
        [ P.onClick \_ ->
            M.mkCont \step -> do
              { cancel } <- repeatedly (step $ _count %~ (_ + 1))
              step (_streamState .~ Streaming { cancel })
        , buttonStyle
        ]
        [ E.text "start streaming"
        ]
      Streaming { cancel } ->
        E.button
        [ P.onClick \_ ->
            M.mkEff (cancel $> (_streamState .~ NotStreaming))
        , buttonStyle
        ]
        [ E.text "stop streaming"
        ]
  ]

  where

  _count = prop (Proxy :: Proxy "count")
  _streamState = prop (Proxy :: Proxy "streamState")

  buttonStyle :: forall m s. P.Prop m s
  buttonStyle = P.style'
    [ S.borderRadius "0"
    , S.borderColor "red"
    ]


type Textbox = String

renderTextbox :: Textbox -> E.Html' Textbox
renderTextbox str =
  E.div
  []
  [ E.p
    []
    [ E.input [ P.type_ "text", P.value str, P.onInput' \val -> M.mkPure (const val) ]
    , E.text " "
    , E.input [ P.type_ "text", P.value str, P.onInput' \val -> M.mkPure (const val) ]
    , E.text " "
    , E.input [ P.type_ "text", P.value str, P.onInput' \val -> M.mkPure (const val) ]
    ]
  , E.p [] [ E.text "As text: ", E.text str ]
  , E.p [] [ E.text "As html: ", E.rawHtml str ]
  , E.p [] [ E.text "As placeholder (!): ", E.input [ P.placeholder str ] ]
  ]


type PhoneNumber = Int /\ Int /\ Int /\ Int /\ Int /\ Int /\ Int /\ Int /\ Int /\ Int

renderPhoneNumber :: PhoneNumber -> E.Html' PhoneNumber
renderPhoneNumber pn =
  E.div
  [ P.style'
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

  digit :: Lens' PhoneNumber Int -> PhoneNumber -> E.Html' PhoneNumber
  digit len pn =
    E.select
    [ P.onInput' $ parseInt >>> (len .~ _) >>> M.mkPure
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
  { counter1 :: Counter
  , counter2 :: Counter
  , textbox :: String
  , checkbox :: Boolean
  , phoneNumber :: PhoneNumber
  }

initial :: Model
initial =
  { counter1: { count: 0, streamState: NotStreaming }
  , counter2: { count: 0, streamState: NotStreaming }
  , textbox: "type in me"
  , checkbox: false
  , phoneNumber: 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0
  }

render :: Model -> E.Html' Model
render model =
  E.div
  []
  [ E.p [] [ M.enroot _counter1 (renderCounter model.counter1) ]
  , E.p [] [ M.enroot _counter2 (renderCounter model.counter2) ]
  , E.hr []
  , M.enroot _textbox (renderTextbox model.textbox)
  , E.hr []
  , E.div
    [ P.style' [ S.fontSize ".8em" ] ]
    [ flip foldMap (range 1 5) \n ->
        E.p [] [ E.text $ show n <> ": monidal `Html` is great!" ]
    ]
  , E.hr []
  , E.enroot _checkbox $ model.checkbox # \checked ->
      E.div
      [] $ (_ `power` 20)
        [ E.input [ P.type_ "checkbox", P.checked      checked , P.onInput \_ -> M.mkPure not ]
        , E.input [ P.type_ "checkbox", P.checked (not checked), P.onInput \_ -> M.mkPure not ]
        ]
  , E.hr []
  , E.enroot _phoneNumber $ renderPhoneNumber model.phoneNumber
  , E.enroot _phoneNumber $ renderPhoneNumber model.phoneNumber
  ]

  where

  _counter1 = prop (Proxy :: Proxy "counter1")
  _counter2 = prop (Proxy :: Proxy "counter2")
  _textbox = prop (Proxy :: Proxy "textbox")
  _checkbox = prop (Proxy :: Proxy "checkbox")
  _phoneNumber = prop (Proxy :: Proxy "phoneNumber")


