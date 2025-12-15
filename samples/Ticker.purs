module Mation.Samples.Ticker where

import Prelude

import Effect (Effect)
import Data.Foldable (intercalate)
import Data.Lens.Setter ((%~), (.~))
import Data.Functor.Contravariant (cmap)
import Data.Int as Int
import Data.Array as Array

import Mation as M
import Mation (Html')
import Mation.Elems as E
import Mation.Props as P
import Mation.Core.Refs (Modify, ReadWrite, ReadWriteL)
import Mation.Core.Refs as Refs
import Mation.Lenses (field)


type Model =
  { rate :: Int
  , count :: Int
  }

initial :: Model
initial =
  { rate: 2
  , count: 0
  }

daemon :: ReadWriteL Model -> Effect Unit
daemon ref = do

  -- Kick off initial ticker
  (cancelTickerRef :: ReadWrite (Effect Unit))
    <- startTicker initial.rate >>= Refs.make

  -- When rate changes, cancel current ticker and start a new one
  ref # Refs.onChange' \{ old, new } -> do
    when (old.rate /= new.rate) do
      -- Cancel existing ticker
      cancelTicker <- Refs.read cancelTickerRef
      cancelTicker
      -- Create new ticker if appropriate
      when (new.rate > 0) do
        newCancel <- startTicker new.rate
        Refs.write newCancel cancelTickerRef

  where

  startTicker :: Int -> Effect (Effect Unit)
  startTicker rate =
    everyNSeconds (1.0 / Int.toNumber rate) do
      ref # Refs.modify (field @"count" %~ add one)

foreign import everyNSeconds :: Number -> Effect Unit -> Effect (Effect Unit)


render :: Model -> Html' (Modify Model)
render model =
  E.div []
  [ E.p []
    [ E.text $ "Currently ticking at a rate of " <> show model.rate <> " ticks per second"
    , E.text ". "
    ]
  , E.p []
    [ E.button [ P.onClick \_ -> changeRate 1 ] [ E.text "Faster!" ]
    , E.text " "
    , E.button [ P.onClick \_ -> changeRate (-1) ] [ E.text "Slower!" ]
    ]
  , Array.replicate model.count (E.text "Tick!") # intercalate (E.text " ")
  ]

  where

  changeRate :: Int -> Modify Model -> Effect Unit
  changeRate delta =
    Refs.modify (
          ( field @"rate" %~ (add delta >>> max 0) )
      >>> ( field @"count" .~ 0 )
    )


main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon
    }

