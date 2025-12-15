module Mation.Samples.Components where

import Prelude
import Effect (Effect)
import Data.Foldable (fold)
import Data.Monoid (power)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Lens (Lens', (^.))
import Data.Lens.Lens (lens)
import Data.Lens.Setter ((.~))
import Data.Lens.Lens.Tuple (_1, _2)

import Mation as M
import Mation (cmap)
import Mation.Core.Refs as Refs
import Mation.Elems as E
import Mation.Props as P
import Mation.Lenses (field)


-- Each component is written the same as a miniature application,
-- with its own model type and render function


-- Component one consists of two string inputs and displays
-- their concatenation

type ConcatModel = String /\ String

initialConcat :: ConcatModel
initialConcat = "unde" /\ "niably"

renderConcat :: ConcatModel -> E.Html' (M.Modify ConcatModel)
renderConcat (prefix /\ suffix) =
  fold
  [ E.input
    [ P.onInputValue \newPrefix -> M.modify (\(_oldPrefix /\ curSuffix) -> newPrefix /\ curSuffix)
    , P.value prefix
    , P.addCss "width: 10ch"
    ]
  , E.text " + "
  , E.input
    [ P.onInputValue \newSuffix -> M.modify (\(curPrefix /\ _oldSuffix) -> curPrefix /\ newSuffix)
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

renderRepeat :: RepeatModel -> E.Html' (M.Modify RepeatModel)
renderRepeat (string /\ count) =
  fold
  [ E.input
    [ P.onInputValue \s -> M.modify (_1 .~ s)
    , P.value string
    , P.addCss "width: 10ch"
    ]
  , E.text " × "
  , E.input
    [ P.type_ "number"
    , P.onInputValue \s -> M.modify (_2 .~ parseNumber s)
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

renderBoth :: BothModel -> E.Html' (M.Modify BothModel)
renderBoth { concat, repeat } =
  fold
  [ E.p [] [ cmap (M.focusWithLens _concat) (renderConcat concat) ]
  , E.p [] [ cmap (M.focusWithLens _repeat) (renderRepeat repeat) ]
  ]

  where

  -- These are lenses which act as witnesses to the components' models
  -- being a part of the application model.
  -- These lenses are what allow component composition!

  _concat = field @"concat"
  _repeat = field @"repeat"



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

renderSharing :: SharingModel -> E.Html' (M.Modify SharingModel)
renderSharing model =
  fold
  [ E.p [] [ (cmap (M.focusWithLens _concat)) (renderConcat $ model ^. _concat) ]
  , E.p [] [ (cmap (M.focusWithLens _repeat)) (renderRepeat $ model ^. _repeat) ]
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

render :: Model -> E.Html' (M.Modify Model)
render (both /\ sharing) =
  E.div
  [ P.addCss "font-family: sans-serif; line-height: 1.5em"
  ]
  [ cmap (M.focusWithLens _1) $ renderBoth both
  , E.hr []
  , cmap (M.focusWithLens _2) $ renderSharing sharing
  ]


main :: Effect Unit
main = M.runApp
  { initial
  , render: render >>> cmap Refs.downcast
  , root: M.underBody
  , daemon: mempty
  }

foreign import parseNumber :: String -> Int
