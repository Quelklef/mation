module Mation.Example where

import Mation.Core.Prelude

import Data.Array (range)

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S

foreign import repeatedly :: Effect Unit -> Effect { cancel :: Effect Unit }

type Counter = { count :: Int, streamState :: StreamState }

data StreamState = NotStreaming | Streaming { cancel :: Effect Unit }

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
    [ E.input
      [ P.type_ "text"
      , P.value str
      , P.onInput' \val -> M.mkPure (const val)
      ]
    ]
  , E.p [] [ E.text "As text: ", E.text str ]
  , E.p [] [ E.text "As html: ", E.rawHtml str ]
  ]


type Model =
  { counter1 :: Counter
  , counter2 :: Counter
  , textbox :: String
  }

render :: Model -> E.Html' Model
render model =
  E.div
  []
  [ E.hr []
  , M.enroot _counter1 (renderCounter model.counter1)
  , E.br []
  , M.enroot _counter2 (renderCounter model.counter2)
  , E.hr []
  , M.enroot _textbox (renderTextbox model.textbox)
  , E.hr []
  , E.div
    [ P.style' [ S.fontSize "0.35em" ] ]
    [ flip foldMap (range 1 10) \n ->
        E.p [] [ E.text $ show n <> ": monidal `Html` is great!" ]
    ]
  ]

  where

  _counter1 = prop (Proxy :: Proxy "counter1")
  _counter2 = prop (Proxy :: Proxy "counter2")
  _textbox = prop (Proxy :: Proxy "textbox")


main :: Effect Unit
main = do
  M.runApp
    { initial:
        { counter1: { count: 0, streamState: NotStreaming }
        , counter2: { count: 0, streamState: NotStreaming }
        , textbox: "type in me"
        }
    , render
    , kickoff: mempty
    , listen: \_ -> pure unit
    , toEffect: identity
    , root: M.useBody
    }

