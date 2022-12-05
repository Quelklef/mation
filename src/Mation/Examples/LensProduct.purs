module Mation.Examples.LensProduct where

import Prelude

import Type.Proxy (Proxy (..))
import Data.Lens.Record (prop)
import Data.Lens ((.~), (%~), lens, Lens', view)

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S


type Box =
  { value :: String
  , globalCounter :: Int
  }

renderBox :: Box -> E.Html' Box
renderBox model =
  E.div
  [ P.style'
    [ S.border "1px solid black"
    , S.padding "1em"
    ]
  ]
  [ E.p
    []
    [ E.input
      [ P.value model.value
      , P.onInput' \val -> M.mkPure (_value .~ val)
      ]
    ]
  , E.p
    []
    [ E.text model.value ]
  , E.p
    []
    [ E.button
      [ P.onClick \_ -> M.mkPure (_globalCounter %~ (_ + 1))
      ]
      [ E.text "increment global counter" ]
    ]
  ]

  where

  _value = prop (Proxy :: Proxy "value")
  _globalCounter = prop (Proxy :: Proxy "globalCounter")


type Model =
  { counter :: Int
  , box1 :: String
  , box2 :: String
  }

initial :: Model
initial =
  { counter: 0
  , box1: "type in me"
  , box2: "type in me as well"
  }

render :: Model -> E.Html' Model
render model =
  E.div
  []
  [ E.p
    []
    [ E.text $ "Global counter is: " <> show model.counter
    ]
  , let l = mkBoxLens _box1 in E.enroot l (renderBox $ view l model)
  , E.br []
  , let l = mkBoxLens _box2 in E.enroot l (renderBox $ view l model)
  ]

  where

  _box1 = prop (Proxy :: Proxy "box1")
  _box2 = prop (Proxy :: Proxy "box2")

  mkBoxLens :: Lens' Model String -> Lens' Model Box
  mkBoxLens lStr =
    lens
      (\mdl ->
          { value: view lStr mdl
          , globalCounter: mdl.counter
          })
      (\mdl newVal ->
          mdl
            { counter = newVal.globalCounter }
            # (lStr .~ newVal.value))
