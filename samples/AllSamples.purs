module Mation.Samples.AllSamples where

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Styles as S
import Mation.Props as P
import Mation.Core.Util.UnsureEq (class UnsureEq, viaEq)
import Mation.Core.Refs as Refs
import Mation.Additional.Router as R
import Mation.Lenses (field)

import Data.Map as Map

import Mation.Samples.Welcome as Samples.Welcome
import Mation.Samples.Counter as Samples.Counter
import Mation.Samples.Components as Samples.Components
import Mation.Samples.AsyncApiCall as Samples.AsyncApiCall
import Mation.Samples.Styling as Samples.Styling
import Mation.Samples.Clock as Samples.Clock
import Mation.Samples.Inputs as Samples.Inputs
import Mation.Samples.TestingZone as Samples.TestingZone
import Mation.Samples.PerfTest as Samples.PerfTest
import Mation.Samples.Pruning as Samples.Pruning


data Page

  = Welcome
  | Counter
  | Components
  | AsyncApiCall
  | Styling
  | Clock
  | Inputs
  | TestingZone
  | PerfTest
  | Pruning

pages :: Array Page
pages =
  -- Canonical page order. Could be given by a derived Enum instance, but I don't
  -- want to add a new dep just for this module
  [ Welcome
  , Counter
  , Components
  , AsyncApiCall
  , Styling
  , Clock
  , Inputs
  , TestingZone
  , PerfTest
  , Pruning
  ]

separateAfter :: Map Page String
separateAfter = Map.fromFoldable
  [ Welcome /\ "basics"
  , Styling /\ "things"
  , Inputs /\ "testing"
  ]

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
  "Async" -> Just AsyncApiCall
  "Testing-Zone" -> Just TestingZone
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
    { welcome :: Samples.Welcome.Model
    , counter :: Samples.Counter.Model
    , components :: Samples.Components.Model
    , asyncApiCall :: Samples.AsyncApiCall.Model
    , styling :: Samples.Styling.Model
    , clock :: Samples.Clock.Model
    , inputs :: Samples.Inputs.Model
    , testing :: Samples.TestingZone.Model
    , perfTest :: Samples.PerfTest.Model
    , pruning :: Samples.Pruning.Model
    }
  }


render :: Model -> E.Html' (M.Modify' Model)
render model =
  E.html
  []
  [ E.head
    []
    []
  , E.body
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
              cmap (M.focusModify $ field @"submodels" <<< field @"welcome") $
                E.prune "page-welcome" model.submodels.welcome Samples.Welcome.render
            Counter      ->
              cmap (M.focusModify $ field @"submodels" <<< field @"counter") $
                E.prune "page-counter" model.submodels.counter Samples.Counter.render
            Components   ->
              cmap (M.focusModify $ field @"submodels" <<< field @"components") $
                E.prune "page-components" model.submodels.components Samples.Components.render
            AsyncApiCall ->
              cmap (M.focusModify $ field @"submodels" <<< field @"asyncApiCall") $
                E.prune "page-asyncApiCall" model.submodels.asyncApiCall Samples.AsyncApiCall.render
            Styling      ->
              cmap (M.focusModify $ field @"submodels" <<< field @"styling") $
                E.prune "page-styling" model.submodels.styling Samples.Styling.render
            Clock        ->
              cmap (const unit) $
                E.prune "page-clock" model.submodels.clock Samples.Clock.render
            Inputs       ->
              cmap (M.focusModify $ field @"submodels" <<< field @"inputs") $
                E.prune "page-inputs" model.submodels.inputs Samples.Inputs.render
            TestingZone  ->
              cmap (M.focusModify $ field @"submodels" <<< field @"testing") $
                E.prune "page-testing" model.submodels.testing Samples.TestingZone.render
            PerfTest     ->
              cmap (M.focusModify $ field @"submodels" <<< field @"perfTest") $
                E.prune "page-perfTest" model.submodels.perfTest Samples.PerfTest.render
            Pruning      ->
              cmap (M.focusModify $ field @"submodels" <<< field @"pruning") $
                E.prune "page-pruning" model.submodels.pruning Samples.Pruning.render
        ]
      ]
    ]
  ]

  where

  navBar =
    -- FIXME: This prune shouldn't be necessary, but without it
    --        we get flickering on the <select> when hovering
    --        different <options>
    --        Possibly an FF issue. See "select flickering" section
    --        of TestingZone.
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
        [ P.onInputValue \val ref ->
            case unpretty val of
              Nothing -> pure unit
              Just page -> ref # M.modify (field @"page" .~ page)
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
          , Map.lookup page separateAfter # foldMap \label ->
              E.option
                [ P.addCss "color: rgb(100, 100, 100)"
                , P.disabled true
                ]
                [ E.rawHtml "&mdash;"
                , E.text " "
                , E.text label
                , E.text " "
                , E.rawHtml "&mdash;"
                ]
          ]
      ]
    ]

  pageWidth = "800px"


initialize :: Effect Model
initialize = do

  welcome <- Samples.Welcome.initialize
  testing <- Samples.TestingZone.initialize

  page <- R.readRoute router

  pure
    { page
    , submodels:
      { welcome
      , counter: Samples.Counter.initial
      , components: Samples.Components.initial
      , testing
      , asyncApiCall: Samples.AsyncApiCall.initial
      , styling: Samples.Styling.initial
      , clock: Samples.Clock.initial
      , inputs: Samples.Inputs.initial
      , perfTest: Samples.PerfTest.initial
      , pruning: Samples.Pruning.initial
      }
    }


main :: Effect Unit
main = do
  initial <- initialize
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.onHtml
    , daemon: \ref -> do
        Samples.Clock.daemon (ref # Refs.focusReadWriteL (field @"submodels" <<< field @"clock"))
        Samples.TestingZone.daemon (ref # Refs.focusReadWriteL (field @"submodels" <<< field @"testing"))
        (R.sync router) (ref # Refs.focusReadWriteL (field @"page"))
    }


