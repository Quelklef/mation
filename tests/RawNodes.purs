module Mation.Tests.RawNodes where

{- START METADATA

{
  "name": "Raw Nodes",
  "desc": "Tests Mation.Elems.rawNode? I think?",
  "specs": [
    "Typing into upper() should produce upcased text",
    "Typing into iframe() should produce an <iframe> with 'src' set to the inputted text"
  ]
}

END METADATA -}

import Mation.Core.Prelude

import Mation as M
import Mation (Daemon', DomNode)
import Mation.Elems as E
import Mation.Core.Refs as Refs


type RawNodes = Maybe (DomNode /\ DomNode)

daemonRawNodes :: Daemon' RawNodes
daemonRawNodes ref = do
  rn1 <- mkTextRawNode
  rn2 <- mkIframeRawNode
  ref # Refs.write (Just $ rn1 /\ rn2)

foreign import mkTextRawNode :: Effect DomNode
foreign import mkIframeRawNode :: Effect DomNode

-- FIXME: an embedded <iframe> will reload if removed from
--        and then re-added to the DOM by Mation re-renders

renderRawNodes :: forall a. RawNodes -> E.Html' a
renderRawNodes model =
  E.div
  []
  [ case model of
    Nothing -> E.text "..."
    Just (rn1 /\ rn2) -> E.rawNode rn1 <> E.rawNode rn2
  ]

main :: Effect Unit
main = do
  M.runApp
    { initial: Nothing
    , render: renderRawNodes
    , root: M.underBody
    , daemon: daemonRawNodes
    }

