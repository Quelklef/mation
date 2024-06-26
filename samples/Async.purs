module Mation.Samples.AsyncApiCall where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Data.Foldable (fold)
import Data.Lens.Setter ((%~), (.~))

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Core.Util.UnsureEq (class UnsureEq, unsureEq, Unsure (..))
import Mation.Lenses (field)


type Model =
  { input :: String
  , requestStatus :: RequestStatus
  , spinnerTick :: Int
  }

data RequestStatus
  = NotStarted
  | InProgress { cancel :: Effect Unit }
  | Success { result :: String }
  | Failure { reason :: String }

instance UnsureEq RequestStatus where
  unsureEq NotStarted NotStarted = Surely true
  unsureEq (InProgress c) (InProgress c') = unsureEq c c'
  unsureEq (Success a) (Success a') = unsureEq a a'
  unsureEq (Failure a) (Failure a') = unsureEq a a'
  unsureEq _ _ = Surely false


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

render :: Model -> E.Html' (M.Modify Model)
render model =
  E.div
  [ P.addStyles
    [ S.display "flex"
    , S.gap "1em"
    , S.fontFamily "sans-serif"
    ]
  ]
  [ renderStringReverse model
  ]

renderStringReverse :: Model -> E.Html' (M.Modify Model)
renderStringReverse model =
  E.div
  [ P.addCss "flex: 1"
  ]
  [ E.h3 [] [ E.text "String-reverse mock API call" ]
  , E.p
    []
    [ E.text "Some input string: "
    , E.input
      [ P.value model.input
      , P.onInputValue \v -> M.modify (field @"input" .~ v)
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
            [ P.onClick \_ ref -> do
                  cancel
                  ref # M.modify (field @"requestStatus" .~ NotStarted)
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
          [ P.addStyles
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
  [ P.addStyles
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
    [ P.addStyles
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


doReverse :: String -> M.Modify Model -> Effect Unit
doReverse string ref = do

  let fps = 60.0
  { cancel: cancelSpinner } <-
    everyNSeconds (1.0 / fps) do
      ref # M.modify (field @"spinnerTick" %~ (_ + 6))

  { cancel: cancelApi } <-
    launchApiCall
      { onSuccess: \result -> do
          cancelSpinner
          ref # M.modify (field @"requestStatus" .~ Success { result })
      , onFailure: \reason -> do
          cancelSpinner
          ref # M.modify (field @"requestStatus" .~ Failure { reason })
      , input: string
      }

  ref # M.modify (field @"requestStatus" .~ InProgress { cancel: cancelSpinner *> cancelApi })


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
