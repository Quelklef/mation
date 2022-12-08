module Mation.Examples.AllExamples where

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Styles as S
import Mation.Props as P

import Mation.Examples.Counter as Counter
import Mation.Examples.Components as Components
import Mation.Examples.LensProduct as LensProduct
import Mation.Examples.Demo as Demo
import Mation.Examples.TestingZone as TestingZone
import Mation.Examples.PerfTest as PerfTest


data Page = Counter | Components | LensProduct | Demo | TestingZone | PerfTest

pages :: Array Page
pages = [ Counter, Components, LensProduct, Demo, TestingZone, PerfTest ]
  -- Don't want to add a whole dep for Enum for a testing module

derive instance Generic Page _
derive instance Eq Page
instance Show Page where show x = genericShow x



syncPageToUrl :: Page -> Effect Unit
syncPageToUrl = show >>> syncPageToUrl_f

foreign import syncPageToUrl_f :: String -> Effect Unit


getPageFromUrl :: Effect (Maybe Page)
getPageFromUrl = fromString <$> getPageFromUrl_f

  where

  fromString :: String -> Maybe Page
  fromString = case _ of
    "Counter" -> Just Counter
    "Components" -> Just Components
    "LensProduct" -> Just LensProduct
    "Demo" -> Just Demo
    "TestingZone" -> Just TestingZone
    "PerfTest" -> Just PerfTest
    _ -> Nothing

foreign import getPageFromUrl_f :: Effect String


type Model =
  { page :: Page
  , counter :: Counter.Model
  , components :: Components.Model
  , lensProduct :: LensProduct.Model
  , demo :: Demo.Model
  , testing :: TestingZone.Model
  , perfTest :: PerfTest.Model
  }

preinitial :: Model
preinitial =
  { page: Counter
  , counter: Counter.initial
  , components: Components.initial
  , testing: TestingZone.initial
  , lensProduct: LensProduct.initial
  , demo: Demo.initial
  , perfTest: PerfTest.initial
  }


render :: Model -> E.Html' Model
render model =
  E.div
  []
  [ navBar
  , E.div
    [ P.style' [ S.padding "1em" ] ]
    [ case model.page of
        Counter -> E.enroot _counter (Counter.render model.counter)
        Components -> E.enroot _components (Components.render model.components)
        TestingZone -> E.enroot _testing (TestingZone.render model.testing)
        LensProduct -> E.enroot _lensProduct (LensProduct.render model.lensProduct)
        Demo -> E.enroot _demo (Demo.render model.demo)
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
    [ E.text "Examples:"
    , intercalate (E.text " ") $ flip map pages \page ->
        let isCurrent = page == model.page in
        E.span
        [ P.style'
          [ S.cursor "pointer"
          , S.lineHeight "3em"
          , if isCurrent then S.textDecoration "underline" else mempty
          ]
        , P.onClick \_ -> M.mkPure (_page .~ page)
        ]
        [ E.text (show page)
        ]
    ]

  _page = prop (Proxy :: Proxy "page")
  _counter = prop (Proxy :: Proxy "counter")
  _components = prop (Proxy :: Proxy "components")
  _lensProduct = prop (Proxy :: Proxy "lensProduct")
  _demo = prop (Proxy :: Proxy "demo")
  _testing = prop (Proxy :: Proxy "testing")
  _perfTest = prop (Proxy :: Proxy "perfTest")


main :: Effect Unit
main = do
  initial <-
    getPageFromUrl >>= case _ of
      Nothing -> pure $ preinitial
      Just page -> pure $ preinitial { page = page }

  M.runApp
    { initial
    , render
    , root: M.useBody
    , listen: \model -> syncPageToUrl model.page
    , kickoff: mempty
    , toEffect: identity
    }
