module Mation.Samples.AllSamples where

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Styles as S
import Mation.Props as P
import Mation.Core.Util.UnsureEq (class UnsureEq, Unsure (Surely))
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
  unsureEq a b = Surely (a == b)

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


render :: Model -> E.Html' (M.Modify Model)
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
              cmap (M.focusWithLens (field @"submodels" <<< field @"welcome")) $
                E.pruneEq "page-welcome" Samples.Welcome.render model.submodels.welcome
            Counter      ->
              cmap (M.focusWithLens (field @"submodels" <<< field @"counter")) $
                E.pruneEq "page-counter" Samples.Counter.render model.submodels.counter
            Components   ->
              cmap (M.focusWithLens (field @"submodels" <<< field @"components")) $
                E.pruneEq "page-components" Samples.Components.render model.submodels.components
            AsyncApiCall ->
              cmap (M.focusWithLens (field @"submodels" <<< field @"asyncApiCall")) $
                E.pruneUeq "page-asyncApiCall" Samples.AsyncApiCall.render model.submodels.asyncApiCall
            Styling      ->
              cmap (M.focusWithLens (field @"submodels" <<< field @"styling")) $
                E.pruneEq "page-styling" Samples.Styling.render model.submodels.styling
            Clock        ->
              cmap (const unit) $
                E.pruneEq "page-clock" Samples.Clock.render model.submodels.clock
            Inputs       ->
              cmap (M.focusWithLens (field @"submodels" <<< field @"inputs")) $
                E.pruneUeq "page-inputs" Samples.Inputs.render model.submodels.inputs
            TestingZone  ->
              cmap (M.focusWithLens (field @"submodels" <<< field @"testing")) $
                E.pruneUeq "page-testing" Samples.TestingZone.render model.submodels.testing
            PerfTest     ->
              cmap (M.focusWithLens (field @"submodels" <<< field @"perfTest")) $
                E.pruneEq "page-perfTest" Samples.PerfTest.render model.submodels.perfTest
            Pruning      ->
              cmap (M.focusWithLens (field @"submodels" <<< field @"pruning")) $
                E.pruneEq "page-pruning" Samples.Pruning.render model.submodels.pruning
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
    model.page # E.pruneEq "nav" \currentPage ->
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
        Samples.Clock.daemon (ref # Refs.focusWithLens (field @"submodels" <<< field @"clock"))
        Samples.TestingZone.daemon (ref # Refs.focusWithLens (field @"submodels" <<< field @"testing"))
        (R.sync router) (ref # Refs.focusWithLens (field @"page"))
    }


