module Mation.Example where

import Mation.Prelude

import Mation as M

foreign import repeatedly :: Effect Unit -> Effect { cancel :: Effect Unit }

type Counter = { count :: Int, streamState :: StreamState }

data StreamState = NotStreaming | Streaming { cancel :: Effect Unit }

renderCounter :: Counter -> M.Html' Counter
renderCounter model =
  M.elt "div"
  [ ]
  [ M.txt (show model.count)
  , M.txt " "
  , M.elt "button"
    [ M.lis "click" $ M.mkPure (_count %~ (_ + 1))
    ]
    [ M.txt "increment once"
    ]
  , M.txt " "
  , case model.streamState of
      NotStreaming ->
        M.elt "button"
        [ M.lis "click" $
            M.mkCont \step -> do
              { cancel } <- repeatedly (step $ _count %~ (_ + 1))
              step (_streamState .~ Streaming { cancel })
        ]
        [ M.txt "start streaming"
        ]
      Streaming { cancel } ->
        M.elt "button"
        [ M.lis "click" $
            M.mkEff (cancel $> (_streamState .~ NotStreaming))
        ]
        [ M.txt "stop streaming"
        ]
  ]

  where

  _count = prop (Proxy :: Proxy "count")
  _streamState = prop (Proxy :: Proxy "streamState")


type Model =
  { counter1 :: Counter
  , counter2 :: Counter
  }

render :: Model -> M.Html' Model
render model =
  M.elt "div"
  []
  [ M.embed _counter1 (renderCounter model.counter1)
  , M.elt "br" [] []
  , M.embed _counter2 (renderCounter model.counter2)
  ]

  where

  _counter1 = prop (Proxy :: Proxy "counter1")
  _counter2 = prop (Proxy :: Proxy "counter2")


main :: Effect Unit
main = do
  M.runApp
    { initial:
        { counter1: { count: 0, streamState: NotStreaming }
        , counter2: { count: 0, streamState: NotStreaming }
        }
    , render
    , kickoff: mempty
    , toEffect: identity
    , root: M.getBody
    }

