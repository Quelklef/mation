module Mation.Examples.PerfTest where

import Mation.Core.Prelude

import Data.Number as Number

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S


foreign import pow :: Number -> Number -> Number
foreign import toFixed :: Int -> Number -> String
foreign import timeMe :: Effect Unit -> Effect Number


type Model =
  { exp :: Number
  , benchmarks :: Array Number
  }

_exp :: Lens' Model Number
_exp = prop (Proxy :: Proxy "exp")

_benchmarks :: Lens' Model (Array Number)
_benchmarks = prop (Proxy :: Proxy "benchmarks")

initial :: Model
initial =
  { exp: 4.0
  , benchmarks: []
  }

render :: Model -> E.Html' Model
render model =
  E.div
  []
  [ E.p
    []
    [ E.text "Using n="
    , E.input
      [ P.type_ "number"
      , P.onInput' \val ->
            case Number.fromString val of
              Nothing -> M.mkNoop
              Just n -> M.mkPure (_exp .~ n)
      , P.value (show model.exp)
      , P.style' [ S.width "6ch" ]
      ]
    , E.text "."
    , E.text " "
    , E.text $ "Rendering to 2ⁿ - 1 = " <> toFixed 0 (pow 2.0 model.exp - 1.0) <> " nodes"
    ]
  , E.div
    [ P.style'
      [ S.display "flex"
      , S.gap "1em"
      ]
    ]
    [ E.div
      []
      [ E.button
        [ P.onClick \_ -> doBench
        ]
        [ E.text "Benchmark!"
        ]
      ]
    , E.div
      []
      [ flip foldMap model.benchmarks \ms ->
        E.div
        []
        [ E.text $ fold
            [ "Frame took ≈"
            , toFixed 0 ms
            ,"ms (≈"
            , toFixed 1 (1000.0 / ms)
            , " fps)"
            ]
        ]
      ]
    ]
  , E.hr []
  , tree model.exp
  ]


tree :: forall m s. Number -> E.Html m s
tree n =
  if n < 1.0 then mempty
  else
    E.div
    [ P.style'
      [ S.margin "1px"
      , S.padding "1px"
      , S.border "1px solid black"
      ]
      -- Add some attributes to make the test less trivial
    , P.class_ "some-class"
    , P.tabindex "0"
    ]
    [ tree (n - 1.0)
    , tree (n - 1.0)
    ]

doBench :: M.Mation Effect Model
doBench = M.mkCont \step -> do
  step (_benchmarks .~ [])
  nTimes 10 do
    ms <- timeMe (step identity)
    step (_benchmarks %~ (_ <> [ms]))

  where

  nTimes = flip power
