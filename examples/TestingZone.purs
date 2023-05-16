module Mation.Examples.TestingZone where

import Mation.Core.Prelude

import Data.Array (range)
import Data.Map as Map

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Selectors as Sel
import Mation.Selectors ((#<>))
import Mation.Core.Util.WRef as WRef
import Mation.Core.Daemon (Daemon)
import Mation.Core.Daemon as D
import Mation.Core.Dom (DomNode)
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
  [ P.style'
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
    [ P.style'
      [ S.display "inline-block"
      , S.minWidth "10ch"
      ]
    , P.dataset $ range 1 (min model.count 50) # foldMap (\n -> Map.singleton (show n) (show n))
    ]
    [ E.text (show model.count)
    ]
  , E.text " | "
  , E.button
    [ P.onClick \_ -> M.mkPure (_count %~ (_ + 1))
    , buttonStyle
    ]
    [ E.text "+"
    ]
  , E.text " "
  , E.button
    [ P.onClick \_ -> M.mkPure (_count %~ (_ - 1))
    , buttonStyle
    ]
    [ E.text "-"
    ]
  , E.text " "
  , case model.streamState of
      NotStreaming -> fold
        [ E.button
          [ P.onClick \_ ->
              M.mkCont \step -> do
                { cancel } <- repeatedly (step $ _count %~ (_ + 1))
                step (_streamState .~ Streaming { cancel })
          , buttonStyle
          ]
          [ E.text "++"
          ]
        , E.text " "
        , E.button
          [ P.onClick \_ ->
              M.mkCont \step -> do
                { cancel } <- repeatedly (step $ _count %~ ((_ - 1) >>> max 0))
                step (_streamState .~ Streaming { cancel })
          , buttonStyle
          ]
          [ E.text "--"
          ]
        ]
      Streaming { cancel } ->
        E.button
        [ P.onClick \_ ->
            M.mkEff (cancel $> (_streamState .~ NotStreaming))
        , buttonStyle
        ]
        [ E.text "🛑"
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
  , P.remark "phone number input"
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


type RawNodes = Maybe (DomNode /\ DomNode)

daemonRawNodes :: Daemon Effect RawNodes
daemonRawNodes wref = do
  rn1 <- mkTextRawNode
  rn2 <- mkIframeRawNode
  WRef.write (Just $ rn1 /\ rn2) wref

foreign import mkTextRawNode :: Effect DomNode
foreign import mkIframeRawNode :: Effect DomNode

-- FIXME: an embedded <iframe> will reload if removed from
--        and then re-added to the DOM by Mation re-renders

renderRawNodes :: forall a. RawNodes -> E.Html' a
renderRawNodes = case _ of
  Nothing -> E.text "..."
  Just (rn1 /\ rn2) -> E.rawNode rn1 <> E.rawNode rn2


type Model =
  { counter1 :: Counter
  , counter2 :: Counter
  , textbox :: String
  , checkbox :: Boolean
  , phoneNumber :: PhoneNumber
  , rawNodes :: RawNodes
  }

initial :: Model
initial =
  { counter1: { count: 0, streamState: NotStreaming }
  , counter2: { count: 0, streamState: NotStreaming }
  , textbox: "type in me"
  , checkbox: false
  , phoneNumber: 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0
  , rawNodes: Nothing
  }

daemon :: Daemon Effect Model
daemon = do
  D.enroot (prop (Proxy :: Proxy "rawNodes")) daemonRawNodes

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
      [] $ (_ `power` 15)
        [ E.input [ P.type_ "checkbox", P.checked      checked , P.onInput \_ -> M.mkPure not ]
        , E.input [ P.type_ "checkbox", P.checked (not checked), P.onInput \_ -> M.mkPure not ]
        ]
  , E.hr []
  , E.enroot _phoneNumber $ renderPhoneNumber model.phoneNumber
  , E.enroot _phoneNumber $ renderPhoneNumber model.phoneNumber
  , E.hr []
  , renderRawNodes model.rawNodes
  , E.br [] `power` 25
  ]

  where

  _counter1 = prop (Proxy :: Proxy "counter1")
  _counter2 = prop (Proxy :: Proxy "counter2")
  _textbox = prop (Proxy :: Proxy "textbox")
  _checkbox = prop (Proxy :: Proxy "checkbox")
  _phoneNumber = prop (Proxy :: Proxy "phoneNumber")


