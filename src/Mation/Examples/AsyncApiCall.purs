module Mation.Examples.AsyncApiCall where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Type.Proxy (Proxy (..))
import Data.Number as Number
import Data.Maybe (Maybe (..))
import Data.Foldable (fold)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~), (.~))

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S


type Model =
  { input :: String
  , requestStatus :: RequestStatus
  , spinnerTick :: Int
  }

_input = prop (Proxy :: Proxy "input")
_requestStatus = prop (Proxy :: Proxy "requestStatus")
_spinnerTick = prop (Proxy :: Proxy "spinnerTick")

data RequestStatus
  = NotStarted
  | InProgress { cancel :: Effect Unit }
  | Success { result :: String }
  | Failure { reason :: String }

isInProgress :: RequestStatus -> Boolean
isInProgress = case _ of
  InProgress _ -> true
  _ -> false


initial :: Model
initial =
  { input: "some string"
  , requestStatus: NotStarted
  , spinnerTick: 0
  }

render :: Model -> E.Html Effect Model
render model =
  E.div
  [ P.style'
    [ S.display "flex"
    , S.gap "1em"
    , S.fontFamily "sans-serif"
    ]
  ]
  [ renderStringReverse model
  ]

renderStringReverse :: Model -> E.Html Effect Model
renderStringReverse model =
  E.div
  [ P.style "flex: 1"
  ]
  [ E.h3 [] [ E.text "String-reverse mock API call" ]
  , E.p
    []
    [ E.text "Some input string: "
    , E.input
      [ P.value model.input
      , P.onInput' $ (_input .~ _) >>> M.mkPure
      , P.disabled $ isInProgress model.requestStatus
      ]
    ]
  , E.p
    []
    [ E.button
      [ P.onClick \_ -> doReverse model.input
      , P.disabled $ isInProgress model.requestStatus
      ]
      [ E.text "Reverse it!"
      ]
    , fold $ case model.requestStatus of
        InProgress { cancel } ->
          [ E.text " "
          , renderSpinner model.spinnerTick
          , E.text " "
          , E.button
            [ P.onClick \_ -> M.mkEff do
                  cancel
                  pure (_requestStatus .~ NotStarted)
            ]
            [ E.text "Cancel"
            ]
          ]

        _ -> mempty
    ]
    , case model.requestStatus of
        Success { result } ->
          E.p
          []
          [ E.text $ "String reversed is: " <> result
          ]

        Failure { reason } ->
          E.p
          [ P.style'
              [ S.color "red"
              ]
          ]
          [ E.text $ "Server error: " <> reason
          ]

        _ -> mempty
  ]

renderSpinner :: forall m s. Int -> E.Html m s
renderSpinner tick =
  let spinnerContainerSize = 30.0
      spinnerSize = 10.0
  in E.span
  [ P.style'
    [ S.display "inline-block"
    , S.verticalAlign "middle"
    , S.margin $ "0 " <> show (spinnerContainerSize / 2.0) <> "px"
    , S.height $ show spinnerContainerSize <> "px"
    , S.width $ show spinnerContainerSize <> "px"
    , S.backgroundColor "rgb(230, 230, 230)"
    , S.borderRadius "100%"
    , S.position "relative"
    , S.transform $ "rotate(" <> show tick <> "deg)"
    ]
  ]
  [ E.div
    [ P.style'
      [ S.display "inline-block"
      , S.background "black"
      , S.width $ show spinnerSize <> "px"
      , S.height $ show spinnerSize <> "px"
      , S.borderRadius "100%"
      , S.position "absolute"
      , S.left "0"
      , S.top $ show ((spinnerContainerSize - spinnerSize) / 2.0) <> "px"
      ]
    ]
    []
  ]


doReverse :: String -> M.Mation Effect Model
doReverse string =
  M.mkCont \step -> do

    let fps = 60.0
    { cancel: cancelSpinner } <-
      everyNSeconds (1.0 / fps) do
        step (_spinnerTick %~ (_ + 6))

    { cancel: cancelApi } <-
      launchApiCall
        { onSuccess: \result -> do
            cancelSpinner
            step (_requestStatus .~ Success { result })
        , onFailure: \reason -> do
            cancelSpinner
            step (_requestStatus .~ Failure { reason })
        , input: string
        }

    step (_requestStatus .~ InProgress { cancel: cancelSpinner *> cancelApi })


-- Imagine that this is some genuine asynchronous API
launchApiCall ::
  { onSuccess :: String -> Effect Unit
  , onFailure :: String -> Effect Unit
  , input :: String
  } -> Effect { cancel :: Effect Unit }
launchApiCall { onSuccess, onFailure, input } = do

  isCanceled <- Ref.new false

  afterNSeconds 4.0 do
    Ref.read isCanceled >>= case _ of
      true -> pure unit
      false -> do
        r <- randNum
        if r < 0.33 then
          onFailure "Unable to frongle the blobius"
        else
          onSuccess (strReverse input)

  let cancel = Ref.write true isCanceled
  pure { cancel }


foreign import everyNSeconds :: Number -> Effect Unit -> Effect { cancel :: Effect Unit }
foreign import afterNSeconds :: Number -> Effect Unit -> Effect Unit
foreign import randNum :: Effect Number
foreign import strReverse :: String -> String
