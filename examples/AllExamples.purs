module Mation.Examples.AllExamples where

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Styles as S
import Mation.Props as P
import Mation.Router as R
import Mation.Core.Daemon as D
import Mation.Core.Util.UnsureEq (class UnsureEq, viaEq)

import Mation.Examples.Welcome as Examples.Welcome
import Mation.Examples.Counter as Examples.Counter
import Mation.Examples.Components as Examples.Components
import Mation.Examples.AsyncApiCall as Examples.AsyncApiCall
import Mation.Examples.Styling as Examples.Styling
import Mation.Examples.Clock as Examples.Clock
import Mation.Examples.Inputs as Examples.Inputs
import Mation.Examples.TestingZone as Examples.TestingZone
import Mation.Examples.PerfTest as Examples.PerfTest
import Mation.Examples.Pruning as Examples.Pruning


data Page

  -- Demo welcome
  = Welcome

  -- Examples
  | Counter
  | Components
  | AsyncApiCall
  | Styling
  | Clock
  | Inputs

  -- Testing stuff
  | TestingZone
  | PerfTest
  | Pruning

separateAfter :: Array Page
separateAfter = [ Welcome, Inputs ]

pages :: Array Page
pages = [ Welcome, Counter, Components, AsyncApiCall, Styling, Clock, Inputs, TestingZone, PerfTest, Pruning ]
  -- Don't want to add a whole dep for Enum for a testing module

derive instance Generic Page _
derive instance Eq Page
derive instance Ord Page
instance Show Page where show x = genericShow x

instance UnsureEq Page where
  unsureEq = viaEq

pretty :: Page -> String
pretty = case _ of
  Welcome -> "Welcome"
  Counter -> "Counter"
  Components -> "Components"
  AsyncApiCall -> "Async"
  Styling -> "Styling"
  Clock -> "Clock"
  Inputs -> "Inputs"
  TestingZone -> "Testing-Zone"
  PerfTest -> "Perf-Test"
  Pruning -> "Pruning"

unpretty :: String -> Maybe Page
unpretty = case _ of
  "Welcome" -> Just Welcome
  "Counter" -> Just Counter
  "Components" -> Just Components
  "Testing-Zone" -> Just TestingZone
  "Async" -> Just AsyncApiCall
  "Styling" -> Just Styling
  "Clock" -> Just Clock
  "Inputs" -> Just Inputs
  "Perf-Test" -> Just PerfTest
  "Pruning" -> Just Pruning
  _ -> Nothing

router :: R.Router Page
router = R.mkVirtualRouter { toPath, fromPath, error }
  where

  toPath :: Page -> R.VirtualPath
  toPath page = R.emptyVirtualPath { parts = [pretty page] }

  fromPath :: R.VirtualPath -> Page
  fromPath { parts } = case parts of
    [s] -> unpretty s # fromMaybe Welcome
    _ -> Welcome

  error _ = Welcome



type Model =
  { page :: Page
  , submodels ::
    { welcome :: Examples.Welcome.Model
    , counter :: Examples.Counter.Model
    , components :: Examples.Components.Model
    , asyncApiCall :: Examples.AsyncApiCall.Model
    , styling :: Examples.Styling.Model
    , clock :: Examples.Clock.Model
    , inputs :: Examples.Inputs.Model
    , testing :: Examples.TestingZone.Model
    , perfTest :: Examples.PerfTest.Model
    , pruning :: Examples.Pruning.Model
    }
  }


render :: Model -> E.Html' Model
render model =
  E.body
  [ P.addStyles
    [ S.margin "0"
    ]
  ]
  [ navBar
  , E.div
    [ P.addStyles
      [ S.display "flex"
      , S.justifyContent "center"
      ]
    ]
    [ E.div
      [ P.addStyles
        [ S.width pageWidth
        , S.padding "2em"
        ]
      ]
      [ case model.page of
          Welcome      ->
            E.enroot (prop (Proxy :: Proxy "submodels") <<< prop (Proxy :: Proxy "welcome")) $
              E.prune "page-welcome" model.submodels.welcome Examples.Welcome.render
          Counter      ->
            E.enroot (prop (Proxy :: Proxy "submodels") <<< prop (Proxy :: Proxy "counter")) $
              E.prune "page-counter" model.submodels.counter Examples.Counter.render
          Components   ->
            E.enroot (prop (Proxy :: Proxy "submodels") <<< prop (Proxy :: Proxy "components")) $
              E.prune "page-components" model.submodels.components Examples.Components.render
          AsyncApiCall ->
            E.enroot (prop (Proxy :: Proxy "submodels") <<< prop (Proxy :: Proxy "asyncApiCall")) $
              E.prune "page-asyncApiCall" model.submodels.asyncApiCall Examples.AsyncApiCall.render
          Styling      ->
            E.enroot (prop (Proxy :: Proxy "submodels") <<< prop (Proxy :: Proxy "styling")) $
              E.prune "page-styling" model.submodels.styling Examples.Styling.render
          Clock        ->
            E.enroot (prop (Proxy :: Proxy "submodels") <<< prop (Proxy :: Proxy "clock")) $
              E.prune "page-clock" model.submodels.clock Examples.Clock.render
          Inputs       ->
            E.enroot (prop (Proxy :: Proxy "submodels") <<< prop (Proxy :: Proxy "inputs")) $
              E.prune "page-inputs" model.submodels.inputs Examples.Inputs.render
          TestingZone  ->
            E.enroot (prop (Proxy :: Proxy "submodels") <<< prop (Proxy :: Proxy "testing")) $
              E.prune "page-testing" model.submodels.testing Examples.TestingZone.render
          PerfTest     ->
            E.enroot (prop (Proxy :: Proxy "submodels") <<< prop (Proxy :: Proxy "perfTest")) $
              E.prune "page-perfTest" model.submodels.perfTest Examples.PerfTest.render
          Pruning      ->
            E.enroot (prop (Proxy :: Proxy "submodels") <<< prop (Proxy :: Proxy "pruning")) $
              E.prune "page-pruning" model.submodels.pruning Examples.Pruning.render
      ]
    ]
  ]

  where

  navBar =
    -- FIXME: this prune shouldn't be necessary, but without it
    --        we get flickering on the <select> when hovering
    --        different <options>
    E.prune "nav" model.page \currentPage ->
    E.div
    [ P.addStyles
      [ S.display "flex"
      , S.justifyContent "center"
      , S.backgroundColor "rgb(50, 50, 50)"
      , S.padding "1.5em 0"
      ]
    ]
    [ E.div
      [ P.addStyles
          [ S.width pageWidth
          , S.fontFamily "sans-serif"
          , S.color "white"
          ]
      ]
      [ E.text "Page: "
      , E.select
        [ P.onInputValue \val step ->
            case unpretty val of
              Nothing -> pure unit
              Just page -> step (prop (Proxy :: Proxy "page") .~ page)
        , P.addStyles
          [ S.fontSize "inherit"
          , S.color "inherit"
          , S.fontFamily "inherit"
          , S.background "transparent"
          , S.border "1px solid white"
          , S.marginLeft "1ch"
          , S.padding ".35em .5em"
          ]
        ]
        $
        pages >>= \page ->
          [ E.option
            [ P.addStyles
              [ S.cursor "pointer"
              , S.textAlign "center"
              , S.padding "1em 0"
              ]
            , P.selected (currentPage == page)
            ]
            [ E.text (pretty page)
            ]
          , if page `elem` separateAfter
            then E.option
                [ P.addCss "color: rgb(100, 100, 100)"
                , P.disabled true
                ]
                [ E.rawHtml "&mdash;"
                ]
            else mempty
          ]
      ]
    ]

  pageWidth = "800px"


initialize :: Effect Model
initialize = do

  welcome <- Examples.Welcome.initialize
  testing <- Examples.TestingZone.initialize

  page <- R.readRoute router

  pure
    { page
    , submodels:
      { welcome
      , counter: Examples.Counter.initial
      , components: Examples.Components.initial
      , testing
      , asyncApiCall: Examples.AsyncApiCall.initial
      , styling: Examples.Styling.initial
      , clock: Examples.Clock.initial
      , inputs: Examples.Inputs.initial
      , perfTest: Examples.PerfTest.initial
      , pruning: Examples.Pruning.initial
      }
    }


main :: Effect Unit
main = do
  initial <- initialize
  M.runApp
    { initial
    , render
    , root: M.onBody
    , daemon: fold
        [ D.enroot (prop (Proxy :: Proxy "submodels") <<< prop (Proxy :: Proxy "clock")) Examples.Clock.daemon
        , D.enroot (prop (Proxy :: Proxy "submodels") <<< prop (Proxy :: Proxy "testing")) Examples.TestingZone.daemon
        , D.enroot (prop (Proxy :: Proxy "page")) $ R.sync router
        ]
    }


