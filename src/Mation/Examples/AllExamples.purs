module Mation.Examples.AllExamples where

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Styles as S
import Mation.Props as P

import Mation.Examples.Counter as Counter
import Mation.Examples.Components as Components
import Mation.Examples.LensProduct as LensProduct
import Mation.Examples.TestingZone as TestingZone


data Page = Counter | Components | LensProduct | TestingZone

pages :: Array Page
pages = [ Counter, Components, LensProduct, TestingZone ]
  -- Don't want to add a whole dep for Enum for a testing module

derive instance Generic Page _
derive instance Eq Page
instance Show Page where show x = genericShow x


type Model =
  { page :: Page
  , counter :: Counter.Model
  , components :: Components.Model
  , lensProduct :: LensProduct.Model
  , testing :: TestingZone.Model
  }

initial :: Model
initial =
  { page: Counter
  , counter: Counter.initial
  , components: Components.initial
  , testing: TestingZone.initial
  , lensProduct: LensProduct.initial
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
    [ E.text "Test cases: "
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
  _testing = prop (Proxy :: Proxy "testing")


main :: Effect Unit
main =
  M.runApp'
    { initial
    , render
    , root: M.useBody
    }

