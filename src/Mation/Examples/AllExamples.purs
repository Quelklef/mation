module Mation.Examples.AllExamples where

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Styles as S
import Mation.Props as P
import Mation.Core.Util.WRef as WRef
import Mation.Core.Daemon as D

import Mation.Examples.Welcome as Welcome
import Mation.Examples.Counter as Counter
import Mation.Examples.Components as Components
import Mation.Examples.AsyncApiCall as AsyncApiCall
import Mation.Examples.Styling as Styling
import Mation.Examples.Clock as Clock
import Mation.Examples.TestingZone as TestingZone
import Mation.Examples.PerfTest as PerfTest
import Mation.Examples.Pruning as Pruning


data Page

  -- Demo welcome
  = Welcome

  -- Examples
  | Counter
  | Components
  | AsyncApiCall
  | Styling
  | Clock

  -- Testing stuff
  | TestingZone
  | PerfTest
  | Pruning

separateAfter :: Array Page
separateAfter = [ Welcome, Clock ]

pages :: Array Page
pages = [ Welcome, Counter, Components, AsyncApiCall, Styling, Clock, TestingZone, PerfTest, Pruning ]
  -- Don't want to add a whole dep for Enum for a testing module

derive instance Generic Page _
derive instance Eq Page
derive instance Ord Page
instance Show Page where show x = genericShow x

pretty :: Page -> String
pretty = case _ of
  Welcome -> "Welcome"
  Counter -> "Counter"
  Components -> "Components"
  AsyncApiCall -> "Async-API-call"
  Styling -> "Styling"
  Clock -> "Clock"
  TestingZone -> "Testing-zone"
  PerfTest -> "Perf-test"
  Pruning -> "Pruning"

unpretty :: String -> Maybe Page
unpretty = case _ of
  "Welcome" -> Just Welcome
  "Counter" -> Just Counter
  "Components" -> Just Components
  "Testing-zone" -> Just TestingZone
  "Async-API-call" -> Just AsyncApiCall
  "Styling" -> Just Styling
  "Clock" -> Just Clock
  "Perf-test" -> Just PerfTest
  "Pruning" -> Just Pruning
  _ -> Nothing



syncPageToUrl :: Page -> Effect Unit
syncPageToUrl = pretty >>> syncPageToUrl_f

foreign import syncPageToUrl_f :: String -> Effect Unit

getPageFromUrl :: Effect (Maybe Page)
getPageFromUrl = unpretty <$> getPageFromUrl_f


foreign import getPageFromUrl_f :: Effect String


type Model =
  { page :: Page
  , welcome :: Welcome.Model
  , counter :: Counter.Model
  , components :: Components.Model
  , asyncApiCall :: AsyncApiCall.Model
  , styling :: Styling.Model
  , clock :: Clock.Model
  , testing :: TestingZone.Model
  , perfTest :: PerfTest.Model
  , pruning :: Pruning.Model
  }


render :: Model -> E.Html' Model
render model =
  E.body
  [ P.style "margin: 0"
  ]
  [ navBar
  , E.div
    [ P.style' [ S.padding "1em" ] ]
    [ case model.page of
        Welcome      ->
          E.enroot (prop (Proxy :: Proxy "welcome")) $
            E.prune "page-welcome" Welcome.render model.welcome
        Counter      ->
          E.enroot (prop (Proxy :: Proxy "counter")) $
            E.prune "page-counter" Counter.render model.counter
        Components   ->
          E.enroot (prop (Proxy :: Proxy "components")) $
            E.prune "page-components" Components.render model.components
        AsyncApiCall ->
          E.enroot (prop (Proxy :: Proxy "asyncApiCall")) $
            E.prune "page-asyncApiCall" AsyncApiCall.render model.asyncApiCall
        Styling      ->
          E.enroot (prop (Proxy :: Proxy "styling")) $
            E.prune "page-styling" Styling.render model.styling
        Clock        ->
          E.enroot (prop (Proxy :: Proxy "clock")) $
            E.prune "page-clock" Clock.render model.clock
        TestingZone  ->
          E.enroot (prop (Proxy :: Proxy "testing")) $
            E.prune "page-testing" TestingZone.render model.testing
        PerfTest     ->
          E.enroot (prop (Proxy :: Proxy "perfTest")) $
            E.prune "page-perfTest" PerfTest.render model.perfTest
        Pruning      ->
          E.enroot (prop (Proxy :: Proxy "pruning")) $
            E.prune "page-pruning" Pruning.render model.pruning
    ]
  ]

  where

  navBar =
    E.div
    [ P.style'
        [ S.display "flex"
        , S.gap "1.5em"
        , S.alignItems "center"
        , S.fontFamily "sans-serif"
        , S.padding "0 2em"
        , S.backgroundColor "rgb(50, 50, 50)"
        , S.color "white"
        ]
    ]
    [ E.text "Page:"
    , intercalate (E.text " ") $ pages >>= \page ->
        [ let isCurrent = page == model.page in
          E.span
          [ P.style'
            [ S.cursor "pointer"
            , S.textAlign "center"
            , S.padding "1em 0"
            , if isCurrent then S.textDecoration "underline" else mempty
            ]
          , P.onClick \_ -> M.mkPure (prop (Proxy :: Proxy "page") .~ page)
          ]
          [ E.text (pretty page)
          ]
        , if page `elem` separateAfter
          then E.text " | "
          else mempty
        ]
    ]


initialize :: Effect Model
initialize = do

  page <-
    getPageFromUrl >>= case _ of
      Nothing -> pure Welcome
      Just page -> pure page

  welcome <- Welcome.initialize

  pure
    { page
    , welcome
    , counter: Counter.initial
    , components: Components.initial
    , testing: TestingZone.initial
    , asyncApiCall: AsyncApiCall.initial
    , styling: Styling.initial
    , clock: Clock.initial
    , perfTest: PerfTest.initial
    , pruning: Pruning.initial
    }


kickoff :: M.Mation Effect Model
kickoff =
  M.mkNoop


main :: Effect Unit
main = do
  initial <- initialize
  M.runApp
    { initial
    , render
    , root: M.onBody
    , daemon: fold
        [ D.enroot (prop (Proxy :: Proxy "clock")) Clock.daemon
        , WRef.onChange \model -> syncPageToUrl model.page
        ]
    , toEffect: identity
    }


