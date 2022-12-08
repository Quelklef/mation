module Mation.Examples.AllExamples where

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Styles as S
import Mation.Props as P

import Mation.Examples.Welcome as Welcome
import Mation.Examples.Counter as Counter
import Mation.Examples.Components as Components
import Mation.Examples.LensProduct as LensProduct
import Mation.Examples.TestingZone as TestingZone
import Mation.Examples.PerfTest as PerfTest


data Page

  -- Demo welcome
  = Welcome

  -- Examples
  | Counter
  | Components
  | LensProduct

  -- Testing stuff
  | TestingZone
  | PerfTest

separateAfter :: Array Page
separateAfter = [ Welcome, LensProduct ]

pages :: Array Page
pages = [ Welcome, Counter, Components, LensProduct, TestingZone, PerfTest ]
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
  LensProduct -> "Lens-product"
  TestingZone -> "Testing-zone"
  PerfTest -> "Perf-test"

unpretty :: String -> Maybe Page
unpretty = case _ of
  "Welcome" -> Just Welcome
  "Counter" -> Just Counter
  "Components" -> Just Components
  "Lens-product" -> Just LensProduct
  "Testing-zone" -> Just TestingZone
  "Perf-test" -> Just PerfTest
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
  , lensProduct :: LensProduct.Model
  , testing :: TestingZone.Model
  , perfTest :: PerfTest.Model
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
        Welcome -> E.enroot _welcome (Welcome.render model.welcome)
        Counter -> E.enroot _counter (Counter.render model.counter)
        Components -> E.enroot _components (Components.render model.components)
        TestingZone -> E.enroot _testing (TestingZone.render model.testing)
        LensProduct -> E.enroot _lensProduct (LensProduct.render model.lensProduct)
        PerfTest -> E.enroot _perfTest (PerfTest.render model.perfTest)
    ]
  ]

  where

  navBar =
    E.div
    [ P.style'
        [ S.display "flex"
        , S.gap "1em"
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
            , S.lineHeight "3em"
            , if isCurrent then S.textDecoration "underline" else mempty
            ]
          , P.onClick \_ -> M.mkPure (_page .~ page)
          ]
          [ E.text (pretty page)
          ]
        , if page `elem` separateAfter
          then E.text " | "
          else mempty
        ]
    ]

  _page = prop (Proxy :: Proxy "page")

  _welcome = prop (Proxy :: Proxy "welcome")
  _counter = prop (Proxy :: Proxy "counter")
  _components = prop (Proxy :: Proxy "components")
  _lensProduct = prop (Proxy :: Proxy "lensProduct")
  _demo = prop (Proxy :: Proxy "demo")
  _testing = prop (Proxy :: Proxy "testing")
  _perfTest = prop (Proxy :: Proxy "perfTest")


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
    , lensProduct: LensProduct.initial
    , perfTest: PerfTest.initial
    }


main :: Effect Unit
main = do
  initial <- initialize
  M.runApp
    { initial
    , render
    , root: M.onBody
    , listen: \model -> syncPageToUrl model.page
    , kickoff: mempty
    , toEffect: identity
    }


