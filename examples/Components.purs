
{- | Example of Mation components -}

module Mation.Examples.Components where
  
import Prelude
import Effect (Effect)
import Data.Foldable (fold)
import Data.Monoid (power)
import Type.Proxy (Proxy (..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Lens (Lens', (^.))
import Data.Lens.Lens (lens)
import Data.Lens.Setter ((.~))
import Data.Lens.Lens.Tuple (_1, _2)
import Data.Lens.Record (prop)

import Mation as M
import Mation.Elems as E
import Mation.Props as P


-- Each component is written the same as a miniature application,
-- with its own model type and render function


-- Component one consists of two string inputs and displays
-- their concatenation

type ConcatModel = String /\ String

initialConcat :: ConcatModel
initialConcat = "unde" /\ "niably"

renderConcat :: ConcatModel -> E.Html' ConcatModel
renderConcat (prefix /\ suffix) =
  fold
  [ E.input
    [ P.onInputValue \newPrefix step -> step (\(_oldPrefix /\ curSuffix) -> newPrefix /\ curSuffix)
    , P.value prefix
    , P.addCss "width: 10ch"
    ]
  , E.text " + "
  , E.input
    [ P.onInputValue \newSuffix step -> step (\(curPrefix /\ _oldSuffix) -> curPrefix /\ newSuffix)
    , P.value suffix
    , P.addCss "width: 10ch"
    ]
  , E.text " = "
  , E.text $ prefix <> suffix
  ]


-- Component two consists of a string input and a number input and
-- displays the string repeated a number of times according to the number

-- For this component we use lenses to make writing the event handlers nicer

type RepeatModel = String /\ Int

initialRepeat :: RepeatModel
initialRepeat = "<>"  /\ 4

renderRepeat :: RepeatModel -> E.Html' RepeatModel
renderRepeat (string /\ count) =
  fold
  [ E.input
    [ P.onInputValue \s step -> step (_1 .~ s)
    , P.value string
    , P.addCss "width: 10ch"
    ]
  , E.text " × "
  , E.input
    [ P.type_ "number"
    , P.onInputValue \s step -> step (_2 .~ parseNumber s)
    , P.value (show count)
    , P.addCss "width: 5ch"
    ]
  , E.text " = "
  , E.text $ string `power` count
  ]


-- Now we render the two components together

type BothModel =
  { concat :: ConcatModel
  , repeat :: RepeatModel
  }

initialBoth :: BothModel
initialBoth =
  { concat: initialConcat
  , repeat: initialRepeat
  }

renderBoth :: BothModel -> E.Html' BothModel
renderBoth { concat, repeat } =
  fold
  [ E.p [] [ E.enroot _concat (renderConcat concat) ]
  , E.p [] [ E.enroot _repeat (renderRepeat repeat) ]
  ]

  where

  -- These are lenses which act as witnesses to the components' models
  -- being a part of the application model.
  -- These lenses are what allow component composition!

  _concat = prop (Proxy :: Proxy "concat")
  _repeat = prop (Proxy :: Proxy "repeat")



-- Here's something fun we can do
-- Both components store a string in their state
-- Using lens trickery, we can have them share that string state!

type SharingModel =
  { sharedString :: String
  , suffix :: String
  , count :: Int
  }

initialSharing :: SharingModel
initialSharing =
  { sharedString: "fish"
  , suffix: "stuff"
  , count: 4
  }

renderSharing :: SharingModel -> E.Html' SharingModel
renderSharing model =
  fold
  [ E.p [] [ E.enroot _concat (renderConcat $ model ^. _concat) ]
  , E.p [] [ E.enroot _repeat (renderRepeat $ model ^. _repeat) ]
  ]

  where

  _concat :: Lens' SharingModel ConcatModel
  _concat = lens
      (\{ sharedString: prefix, suffix } -> prefix /\ suffix)
      (\{ count } (prefix /\ suffix) -> { sharedString: prefix, suffix, count })

  _repeat :: Lens' SharingModel RepeatModel
  _repeat =  lens
      (\{ sharedString: string, count } -> string /\ count)
      (\{ suffix } (sharedString /\ count) -> { sharedString, suffix, count })



-- Define the top-level application

type Model = BothModel /\ SharingModel

initial :: Model
initial = initialBoth /\ initialSharing

render :: Model -> E.Html' Model
render (both /\ sharing) =
  E.div
  [ P.addCss "font-family: sans-serif; line-height: 1.5em"
  ]
  [ E.enroot _1 $ renderBoth both
  , E.hr []
  , E.enroot _2 $ renderSharing sharing
  ]


main :: Effect Unit
main = M.runApp' { initial, render, root: M.underBody }

foreign import parseNumber :: String -> Int
