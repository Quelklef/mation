module Mation.Tests.OnClickElsewhere where

{- BEGIN METADATA

{
  "name": "onClickElsewhere",
  "desc": "Tests Mation.Props.onClickElsewhere",
  "specs": [
    "Clicking inside or outside the box should increment the appropriate counter"
  ]
}

END METADATA -}

import Mation.Core.Prelude

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Core.Refs as Refs
import Mation.Lenses (field)


type OnClickElsewhere =
  { here :: Int
  , elsewhere :: Int
  }

viewOnClickElsewhere :: OnClickElsewhere -> E.Html' (M.Modify OnClickElsewhere)
viewOnClickElsewhere { here, elsewhere } =
  E.div
  [ P.onClick \_ -> M.modify (field @"here" %~ (_ + 1))
  , P.onClickElsewhere \_ -> M.modify (field @"elsewhere" %~ (_ + 1))
  , P.addStyles
    [ S.fontFamily "sans-serif"
    , S.padding "1em"
    , S.background "rgb(230, 230, 230)"
    , S.display "inline-block"
    , S.userSelect "none"
    ]
  ]
  [ E.text $ "clicks inside: " <> show here <> " • " <> "clicks outside: " <> show elsewhere
  ]

main :: Effect Unit
main = do
  M.runApp
    { initial: { here: 0, elsewhere: 0 }
    , render: viewOnClickElsewhere >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }

