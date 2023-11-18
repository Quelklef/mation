module Mation.Samples.TestingZone where

import Mation.Core.Prelude

import Data.Array (range)
import Data.Map as Map
import Data.Lens (lens)

import Mation as M
import Mation (Daemon, Daemon')
import Mation.Elems as E
import Mation.Props as P
import Mation.Props.Unsafe as P
import Mation.Styles as S
import Mation.Selectors as Sel
import Mation.Selectors ((#<>))
import Mation.Core.Refs as Refs
import Mation.Core.Dom (DomNode)
import Mation.Core.Util.UnsureEq (class UnsureEq, Unsure (..), viaPrim)
import Mation.Experimental.Component as Com
import Mation.Lenses (field, (×))


--------------------------------------------------------------------------------

type Counter = { count :: Int, streamState :: StreamState }

data StreamState = NotStreaming | Streaming { cancel :: Effect Unit }

-- FIXME: genericUnsureEq not working when the type contains a record?
instance UnsureEq StreamState where
  unsureEq NotStreaming NotStreaming = Surely true
  unsureEq (Streaming c) (Streaming c') = viaPrim c c'
  unsureEq _ _ = Surely false

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
              { cancel } <- everyNSeconds 0.05 (ref # M.modify (_count %~ ((_ - 1) >>> max 0)))
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


--------------------------------------------------------------------------------

type Textbox = String

renderTextbox :: Textbox -> E.Html' (M.Write Textbox)
renderTextbox str =
  E.div
  []
  [ E.p
    []
    [ E.input [ P.type_ "text", P.value str, P.onInputValue M.write ]
    , E.text " "
    , E.input [ P.type_ "text", P.value str, P.onInputValue M.write ]
    , E.text " "
    , E.input [ P.type_ "text", P.value str, P.onInputValue M.write ]
    ]
  , E.p [] [ E.text "As text: ", E.text str ]
  , E.p [] [ E.text "As html: ", E.rawHtml str ]
  , E.p [] [ E.text "As placeholder (!): ", E.input [ P.placeholder str ] ]
  ]


--------------------------------------------------------------------------------

type PhoneNumber = Int /\ Int /\ Int /\ Int /\ Int /\ Int /\ Int /\ Int /\ Int /\ Int

renderPhoneNumber :: PhoneNumber -> E.Html' (M.Modify PhoneNumber)
renderPhoneNumber = \pn ->
  E.div
  [ P.addStyles
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


--------------------------------------------------------------------------------

type OnClickElsewhere =
  { here :: Int
  , notHere :: Int
  }

viewOnClickElsewhere :: OnClickElsewhere -> E.Html' (M.Modify OnClickElsewhere)
viewOnClickElsewhere { here, notHere } =
  E.div
  [ P.onClick \_ -> M.modify (field @"here" %~ (_ + 1))
  , P.onClickElsewhere \_ -> M.modify (field @"notHere" %~ (_ + 1))
  , P.addStyles
    [ S.fontFamily "sans-serif"
    , S.padding "1em"
    , S.border "1px solid black"
    , S.display "inline-block"
    , S.margin "1em 0 1em 25px"
    , S.userSelect "none"
    ]
  ]
  [ E.text $ "clicks inside: " <> show here <> " / " <> "clicks outside: " <> show notHere
  ]

--------------------------------------------------------------------------------

type WithKTest =
  { a :: Int
  , b :: Int
  }

viewWithKTest :: WithKTest -> E.Html' (M.Modify WithKTest)
viewWithKTest { a, b } =
  E.div
  []
  [ E.text "Test for "
  , E.code [] [ E.text "withK" ]
  , E.text ": "
  , E.button
    [ P.onClick \_ -> M.modify (field @"a" %~ (_ + 1))
    ]
    [ E.text ("a value: " <> show a)
    ]
  , E.text " "
  , E.withK \ref ->
    E.button
    [ P.onClick \_ _ -> ref # M.modify (field @"b" %~ (_ + 1))
    ]
    [ E.text ("b value: " <> show b)
    ]
  ]


{- -----------------------------------------------------------------------------

Test: select flickering

Bug: Under certain conditions, if a <select> is patched when it is open then it
will flicker and its over state will be reset, even if it hasn't changed since the
previous frame.

- Sufficient conditions for this to occur are:
    - The <select> is not pruned
    - The <select> has a fixup prop that adds a class via either `.setAttribute('class', ...)`
      or via `.classList.add(...)`. The fixup prop's restoration function need not remove the
      class. (Corollary: the fixup need not be valid!)
- By "hover state" I mean the DOM-native hover state. In chrome, this is a blue
  background behind the hovered <option>

Cause: Repros on Firefox but not Chrome. Potentially an FF issue?

-} --


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
  [ P.addStyles
    [ S.display "grid"
    , S.gridTemplateColumns "200px 100px 100px"
    , S.alignItems "center"
    , S.gap "1em"
    ]
  ]
  [ E.p
    []
    [ E.text "The  ", E.code [] [ E.text "<select>" ], E.text " should not flicker when open"
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
  , E.p
    []
    [ E.text ("Time: " <> show model.time)
    ]
  ]

foreign import addClass :: String -> DomNode -> Effect Unit


--------------------------------------------------------------------------------

type ComStuff =
  { string :: String
  , caps :: Boolean
  }

stringComponent :: Com.Component Effect Unit String
stringComponent =
  Com.mkComponent
    { name: Proxy :: Proxy "left component"
    , init: pure "meow meow meow"
    , daemon: \_ -> pure unit
    , view: \(_ /\ s) ->
        E.input
        [ P.onInputValue \v -> M.modify (_2 .~ v)
        , P.value s
        ]
    }

comComponent :: Com.Component Effect Unit ComStuff
comComponent =
  Com.mkParent1
    { name: Proxy :: Proxy "Component api example"
    , init: \string -> pure { string, caps: false }
    , daemon: \_ -> pure unit
    , view: \stringCom (_ /\ { string, caps }) ->
        E.div
        []
        [ stringCom
        , E.text " "
        , E.label [ P.for "caps" ] [ E.text "caps" ]
        , E.input
          [ P.type_ "checkbox"
          , P.id "caps"
          , P.checked caps
          , P.onInputValue \_ -> M.modify (_2 <<< field @"caps" %~ not)
          ]
        , E.text " → "
        , E.text (if caps then toUpperCase string else string)
        ]
    , child1:
      { component: stringComponent
      , at: Com.BoxLens (_1 × _2 <<< field @"string")
      }
    }

foreign import toUpperCase :: String -> String


--------------------------------------------------------------------------------

type RawNodes = Maybe (DomNode /\ DomNode)

daemonRawNodes :: Daemon' RawNodes
daemonRawNodes ref = do
  rn1 <- mkTextRawNode
  rn2 <- mkIframeRawNode
  Refs.write (Just $ rn1 /\ rn2) ref

foreign import mkTextRawNode :: Effect DomNode
foreign import mkIframeRawNode :: Effect DomNode

-- FIXME: an embedded <iframe> will reload if removed from
--        and then re-added to the DOM by Mation re-renders

renderRawNodes :: forall a. RawNodes -> E.Html' a
renderRawNodes = case _ of
  Nothing -> E.text "..."
  Just (rn1 /\ rn2) -> E.rawNode rn1 <> E.rawNode rn2


--------------------------------------------------------------------------------
-- Toplevel logic

type Model =
  { counter1 :: Counter
  , counter2 :: Counter
  , textbox :: String
  , checkbox :: Boolean
  , phoneNumber :: PhoneNumber
  , onClickElsewhere :: OnClickElsewhere
  , withKTest :: WithKTest
  , selectFlicker :: SelectFlicker
  , comStuff :: ComStuff
  , rawNodes :: RawNodes
  }

initialize :: Effect Model
initialize = do
  comStuff <- Com.initializeC comComponent
  pure
    { counter1: { count: 0, streamState: NotStreaming }
    , counter2: { count: 0, streamState: NotStreaming }
    , textbox: "type in me"
    , checkbox: false
    , phoneNumber: 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0 /\ 0
    , onClickElsewhere: { here: 0, notHere: 0 }
    , withKTest: { a: 0, b: 0 }
    , selectFlicker: initialSelectFlicker
    , comStuff
    , rawNodes: Nothing
    }

daemon :: Daemon Effect Model
daemon ref = do
  daemonRawNodes (ref # Refs.focusWithLens (field @"rawNodes"))
  daemonSelectFlicker (ref # Refs.focusWithLens (field @"selectFlicker"))
  (Com.daemonC comComponent) (ref # Refs.focusWithLens (_unit × field @"comStuff"))

render :: Model -> E.Html' (M.Modify Model)
render model =
  E.div
  []
  [ E.p [] [ cmap (M.focusWithLens _counter1) (renderCounter model.counter1) ]
  , E.p [] [ cmap (M.focusWithLens _counter2) (renderCounter model.counter2) ]
  , E.hr []
  , cmap (M.focusWithLens _textbox >>> Refs.downcast) (renderTextbox model.textbox)
  , E.hr []
  , E.div
    [ P.addStyles [ S.fontSize ".8em" ] ]
    [ flip foldMap (range 1 5) \n ->
        E.p [] [ E.text $ show n <> ": monidal `Html` is great!" ]
    ]
  , E.hr []
  , E.div
    [ P.addStyles
      -- v Test: that later styles get precedence
      [ S.display "block"
      , S.display "none"
      ]
    ]
    [ E.div
      [ P.addStyles
        [ S.backgroundColor "red"
        , S.color "white"
        , S.fontSize "4em"
        ]
      ]
      [ E.text "uh oh something worng"
      ]
    , E.hr []
    ]
  , cmap (M.focusWithLens _checkbox) $ model.checkbox # \checked ->
      E.div
      [] $ (_ `power` 15)
        [ E.input [ P.type_ "checkbox", P.checked      checked , P.onInput \_ -> M.modify not ]
        , E.input [ P.type_ "checkbox", P.checked (not checked), P.onInput \_ -> M.modify not ]
        ]
  , E.hr []
  , cmap (M.focusWithLens _phoneNumber) $ renderPhoneNumber model.phoneNumber
  , cmap (M.focusWithLens _phoneNumber) $ renderPhoneNumber model.phoneNumber
  , E.hr []
  , cmap (M.focusWithLens _onClickElsewhere) $ viewOnClickElsewhere model.onClickElsewhere
  , E.hr []
  , cmap (M.focusWithLens _withKTest) $ viewWithKTest model.withKTest
  , E.hr []
  , cmap (M.focusWithLens _selectFlicker) $ viewSelectFlicker model.selectFlicker
  , E.hr []
  , cmap (M.focusWithLens (_unit × _comStuff)) $ Com.viewC comComponent (unit /\ model.comStuff)
  , E.hr []
  , renderRawNodes model.rawNodes
  , E.br [] `power` 25
  ]

  where

  _counter1 = field @"counter1"
  _counter2 = field @"counter2"
  _textbox = field @"textbox"
  _checkbox = field @"checkbox"
  _phoneNumber = field @"phoneNumber"
  _onClickElsewhere = field @"onClickElsewhere"
  _withKTest = field @"withKTest"
  _selectFlicker = field @"selectFlicker"
  _comStuff = field @"comStuff"

_unit :: forall x. Lens' x Unit
_unit = lens (const unit) (\v _ -> v)

