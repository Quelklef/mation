module Mation.Examples.Pruning where

import Mation.Core.Prelude

import Data.Number as Number
import Data.Lens.Lens.Tuple (_1, _2)

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Core.Util.UnsureEq (class UnsureEq)


type Double a = a /\ a

type Vals = Double (Double (Double (Double Int)))

type Model =
  { vals :: Vals
  }

_vals = prop (Proxy :: Proxy "vals")

initial :: Model
initial =
  { vals: double (double (double (double 0)))
  }

  where

  double :: forall a. a -> Double a
  double x = x /\ x


onDouble :: forall a. UnsureEq a => (a -> E.Html' a) -> (Double a -> E.Html' (Double a))
onDouble render model =
  E.div
  [ P.style'
    [ S.display "inline-block"
    , S.border "1px solid lightgrey"
    , S.padding ".2em"
    , S.margin ".2em"
    ]
  , P.showUpdates
  ]
  [ E.prune Nothing (\n -> E.enroot _1 $ render n) (model ^. _1)
  , E.prune Nothing (\n -> E.enroot _2 $ render n) (model ^. _2)
  ]


render :: Model -> E.Html' Model
render model =

  E.div
  []
  [ E.p [] [ E.text "Test page for the (WIP) FastEnroot API, which allows for true partial updates in a Mation application: the Virtual DOM won't even be recomputed unless the relevant part of the model changes. In this demo when the application re-renders it will light up the parts of the DOM which were touched during the diffing process, visualizing what work is and is not being done." ]
  , E.hr []
  , E.enroot _vals (renderVals model.vals)
  ]

  where

  renderVals :: Vals -> E.Html' Vals
  renderVals vals =
    E.div
    []
    [ E.div
      [ P.style "display: flex; gap: 1em; flex-wrap: wrap"
      ]
      [ E.enroot (_1 <<< _1 <<< _1 <<< _1) $ mkCounter (vals ^. (_1 <<< _1 <<< _1 <<< _1))
      , E.enroot (_1 <<< _1 <<< _1 <<< _2) $ mkCounter (vals ^. (_1 <<< _1 <<< _1 <<< _2))
      , E.enroot (_1 <<< _1 <<< _2 <<< _1) $ mkCounter (vals ^. (_1 <<< _1 <<< _2 <<< _1))
      , E.enroot (_1 <<< _1 <<< _2 <<< _2) $ mkCounter (vals ^. (_1 <<< _1 <<< _2 <<< _2))
      , E.enroot (_1 <<< _2 <<< _1 <<< _1) $ mkCounter (vals ^. (_1 <<< _2 <<< _1 <<< _1))
      , E.enroot (_1 <<< _2 <<< _1 <<< _2) $ mkCounter (vals ^. (_1 <<< _2 <<< _1 <<< _2))
      , E.enroot (_1 <<< _2 <<< _2 <<< _1) $ mkCounter (vals ^. (_1 <<< _2 <<< _2 <<< _1))
      , E.enroot (_1 <<< _2 <<< _2 <<< _2) $ mkCounter (vals ^. (_1 <<< _2 <<< _2 <<< _2))
      , E.enroot (_2 <<< _1 <<< _1 <<< _1) $ mkCounter (vals ^. (_2 <<< _1 <<< _1 <<< _1))
      , E.enroot (_2 <<< _1 <<< _1 <<< _2) $ mkCounter (vals ^. (_2 <<< _1 <<< _1 <<< _2))
      , E.enroot (_2 <<< _1 <<< _2 <<< _1) $ mkCounter (vals ^. (_2 <<< _1 <<< _2 <<< _1))
      , E.enroot (_2 <<< _1 <<< _2 <<< _2) $ mkCounter (vals ^. (_2 <<< _1 <<< _2 <<< _2))
      , E.enroot (_2 <<< _2 <<< _1 <<< _1) $ mkCounter (vals ^. (_2 <<< _2 <<< _1 <<< _1))
      , E.enroot (_2 <<< _2 <<< _1 <<< _2) $ mkCounter (vals ^. (_2 <<< _2 <<< _1 <<< _2))
      , E.enroot (_2 <<< _2 <<< _2 <<< _1) $ mkCounter (vals ^. (_2 <<< _2 <<< _2 <<< _1))
      , E.enroot (_2 <<< _2 <<< _2 <<< _2) $ mkCounter (vals ^. (_2 <<< _2 <<< _2 <<< _2))
      ]
    , E.hr []
    , onDouble (onDouble (onDouble (onDouble mkCounter))) vals
    ]

  mkCounter :: Int -> E.Html' Int
  mkCounter = \val ->
    E.span
    [ P.style'
      [ S.display "inline-flex"
      , S.padding ".2em .5em"
      , S.alignItems "center"
      ]
    ]
    [ E.span
      [ P.style "margin-right: 5px" ]
      [ E.text (show val) ]
    , E.span
      [ P.style'
        [ S.display "inline-flex"
        , S.flexDirection "column"
        ]
      ]
      [ E.button
        [ P.onClick \_ -> M.mkPure (_ + 1) ]
        [ E.text "↑" ]
      , E.button
        [ P.onClick \_ -> M.mkPure (_ - 1) ]
        [ E.text "↓" ]
      ]
    ]

