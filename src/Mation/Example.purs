module Mation.Example where

import Mation.Prelude

import Mation as M

foreign import repeatedly :: Effect Unit -> Effect { cancel :: Effect Unit }

type Model =
  { count :: Int
  , streamState :: StreamState
  }

data StreamState
  = NotStreaming
  | Streaming { cancel :: Effect Unit }

initial :: Model
initial =
  { count: 0
  , streamState: NotStreaming
  }


render :: Model -> M.Html' Model
render model =
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


main :: Effect Unit
main = do
  M.runApp
    { initial
    , render
    , kickoff: mempty
    , toEffect: identity
    , root: M.getBody
    }

