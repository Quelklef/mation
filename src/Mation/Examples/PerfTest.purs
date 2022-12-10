module Mation.Examples.PerfTest where

import Mation.Core.Prelude

import Data.Number as Number
import Data.Lens.Lens.Tuple (_2)

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S


foreign import pow :: Number -> Number -> Number
foreign import toFixed :: Int -> Number -> String
foreign import timeMe :: Effect Unit -> Effect Number


type Model =
  { string :: String
  , exp :: Number
  , benchmarks :: Array Number
  , fast :: Boolean
  }


_string :: Lens' Model String
_string = prop (Proxy :: Proxy "string")

_exp :: Lens' Model Number
_exp = prop (Proxy :: Proxy "exp")

_benchmarks :: Lens' Model (Array Number)
_benchmarks = prop (Proxy :: Proxy "benchmarks")

_fast :: Lens' Model Boolean
_fast = prop (Proxy :: Proxy "fast")




initial :: Model
initial =
  { string: ""
  , exp: 4.0
  , benchmarks: []
  , fast: false
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
    , E.text $ "Rendering to 2ⁿ⁺¹-1 = " <> toFixed 0 (pow 2.0 (model.exp + 1.0) - 1.0) <> " nodes"
    ]
  , E.p
    []
    [ E.input
      [ P.type_ "checkbox"
      , P.checked model.fast
      , P.onInput' \_ -> M.mkPure (_fast %~ not)
      , P.id "use-fast"
      ]
    , E.label
      [ P.for "use-fast" ]
      [ E.text " use fast" ]
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
        [ E.text "Benchmark render"
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
-- TODO: is it possible to parameterize enrootFast? :(
--  , if model.fast
--    then E.enrootFast
--            (\mdl -> mdl.exp /\ mdl.string)
--            (\(a /\ b) -> E.enroot _2 $ tree a b)
--            model
--    else E.enroot _string $ tree model.exp model.string
  , E.enroot _string $ tree model.exp model.string
  ]


tree :: Number -> String -> E.Html' String
tree n string =
  if n < 1.0
  then
    E.div
    []
    [ E.input
      [ P.type_ "text"
      , P.value string
      , P.onInput' \val -> M.mkPure (const val)
      , P.style'
        [ S.border "none"
        , S.backgroundColor "rgb(255, 220, 255)"
        ]
      ]
    ]
  else
    E.div
    [ P.style'
      [ S.margin "1px"
      , S.padding "1px"
      , S.border "1px solid black"
      , S.textAlign "center"
      ]
      -- Add some attributes to make the test less trivial
    , P.class_ "some-class"
    ]
    [ tree (n - 1.0) string
    , tree (n - 1.0) string
    ]

doBench :: M.Mation Effect Model
doBench = M.mkStaged \{ stage, apply } -> do
  stage (_benchmarks .~ [])
  nTimes 10 do
    stage identity
    ms <- timeMe apply
    stage (_benchmarks %~ (_ <> [ms]))
  apply

  where

  nTimes = flip power
