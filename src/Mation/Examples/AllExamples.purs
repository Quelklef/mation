module Mation.Examples.AllExamples where

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Styles as S
import Mation.Props as P

import Mation.Examples.Testing as Testing
import Mation.Examples.LensProduct as LensProduct


data Page = Testing | LensProduct

pages :: Array Page
pages = [ Testing, LensProduct ]
  -- Don't want to add a whole dep for Enum for a testing module

derive instance Generic Page _
derive instance Eq Page
instance Show Page where show x = genericShow x


type Model =
  { page :: Page
  , testing :: Testing.Model
  , lensProduct :: LensProduct.Model
  }

initial :: Model
initial =
  { page: Testing
  , testing: Testing.initial
  , lensProduct: LensProduct.initial
  }


render :: Model -> E.Html' Model
render model =
  E.div
  []
  [ navBar
  , case model.page of
      Testing -> E.enroot _testing (Testing.render model.testing)
      LensProduct -> E.enroot _lensProduct (LensProduct.render model.lensProduct)
  ]

  where

  navBar =
    E.div
    []
    [ E.text "Nav: "
    , intercalate (E.text " ") $ flip map pages \page ->
        E.span
        [ P.onClick \_ -> M.mkPure (_page .~ page)
        ]
        [ let isCurrent = page == model.page in
          E.span
          [ P.style'
            [ S.cursor "pointer"
            , if isCurrent then S.fontWeight "bold" else mempty
            ]
          ]
          [ E.text (show page)
          ]
        ]
    ]

  _page = prop (Proxy :: Proxy "page")
  _testing = prop (Proxy :: Proxy "testing")
  _lensProduct = prop (Proxy :: Proxy "lensProduct")


main :: Effect Unit
main =
  M.runApp'
    { initial
    , render
    , root: M.useBody
    }
