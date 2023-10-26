module Mation.Samples.Kittens where

{- First thing, imports -}

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Control.Promise (Promise, toAffE)
import Data.Int as Int
import Data.Maybe (Maybe (..))
import Data.Lens (Lens')
import Data.Foldable (fold, intercalate)
import Data.String.CodeUnits (toCharArray, singleton) as String
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)

import Mation (Html, Html', Style)
import Mation.Run (runApp, onBody)
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Selectors ((#>>), (#<>))
import Mation.Selectors as Sel
import Mation.Lenses (field)
import Mation.Core.Util.UnsureEq (class UnsureEq, genericUnsureEq)


{- The first component we will build is for the sliders. We package
all the slider logic into a component because we will use it multiple
times (once for each of our two sliders).

Our component consists of two things:

1. A type, called the component's "model type" -}

type SliderModel = Int

{- The model type tells us what the state of the component is. A slider
needs to track only one thing, its value. Its value is an integer, so the
component type is Int.

2. The second part of this component is its render function (also called
its "view"). This decides both how the slider will be displayed and how
it will behave when a user interacts with it. -}

renderSlider :: SliderModel -> Html' SliderModel

{- The component render function takes as input the current state of the
component and produces a value of type `Html' SliderModel`. Here
the `Html'` tells us that it is returning a representation of some HTML,
and the argument `SliderModel` tells us that the HTML, when interacted
with, wants to update some state of type `SliderModel` -}

renderSlider value =
  {- Each slider component renders to a <span> containing some stuff. HTML
  nodes are created with functions from Mation.Elems (here called E), such
  as E.span. The function E.span takes two arrays. The first contains
  its "props", which are HTML attributes, even listeners, and the like. The
  second array contains the node children. -}
  E.span
  []
  [ E.input
    {- Setting some HTML attributes on the rendered <input> -}
    [ P.type_ "range"
    , P.min "0"
    , P.step "1"
    , P.max "600"
    , P.value (show value)
    , P.addStyles [ S.verticalAlign "middle" ]  {- Just a touch of CSS -}
    {- Ah, an event listener! Event listeners are effectful functions,
    typically of two arguments. The first argument is an incoming value,
    such as a DOM event or a string. The second argument is an "update
    function" which allows the listener to modify the component model. -}
    , P.onInputValue \
          (newValue :: String)
            {- ^ This is the new value of the <input> -}
          (updateModel :: (SliderModel -> SliderModel) -> Effect Unit)
            {- ^ This, when called with a function, will apply that
            function to the component's current state, updating it. -}
        -> do
          {- The return value of the listener is an `Effect Unit`, which gives
          it a lot of power. Here we just do something simple: we turn
          the new <input> value into a number and then update the model. -}
          case Int.fromString newValue of
            Nothing ->
              {- Failed to parse; do nothing -}
              pure unit
            Just (parsedInputValue :: Int) ->
              {- Parse success; update model -}
              updateModel \_oldNumber -> parsedInputValue
    ]

  {- After the <input> we display the component current value -}
  , E.text " "
  , E.text (show value <> "px")
  ]

{- This completes the first component. All it consists of is its model type
and a way to render it. The render function also handles behaviour.

To be made use of, another component will embed this component by giving a
place for both its model and rendered `Html` to live. -}


{- Now for component number two! This will be our primary component and
consist of both width and height sliders (as child components) plus
the button and actual generated image.

First we define its model. -}

type CatGeneratorModel =
  { catWidth :: Int
  , catHeight :: Int
  , catImage :: CatImage
  }

data CatImage = NoImage | Loading | CatHtml { rawHtml :: String }
derive instance Eq CatImage

{- The `catWidth` and `catHeight` fields will be the state for the
child slider components. In Mation, every child component model
is stored within its parent component model. This means that the
state for the entire app is centralized in one place: the top-level
component. -}

{- Next we define some lenses. We'll use these later. If you don't
know what a lens is, think of it like a first-class record field.
We'll use these to tell Mation where to embed the child components. -}

_catWidth :: Lens' CatGeneratorModel Int
_catWidth = field @"catWidth"

_catHeight :: Lens' CatGeneratorModel Int
_catHeight = field @"catHeight"

_catImage :: Lens' CatGeneratorModel CatImage
_catImage = field @"catImage"

{- Here we define our render function for our main component. This has
the same form as before: `SomeModel -> Html' SomeModel` -}

renderCatGenerator :: CatGeneratorModel -> Html' CatGeneratorModel
renderCatGenerator model =
  E.div
  []
  [ E.p
    []
    [ E.text "Width: "
      {- Here is where we embed the first slider. To do this,
      we first fetch its state from *our* state via `model.catWidth`. We
      feed this into the slider component's render function to produce
      its `Html' SliderModel`. We can't use this as-is, because what we're
      trying to produce is an `Html' CatGeneratorModel`: the state types
      don't match up. To fix this, we use `enroot _catWidth` to transform
      the returned `Html' SliderModel` into an `Html' CatGeneratorModel`. -}
    , E.enroot _catWidth $ renderSlider model.catWidth
    ]
  , E.p
    []
    [ E.text "Height: "
      {- Embed the other slider component -}
    , E.enroot _catHeight $ renderSlider model.catHeight
    ]
  , E.p
    []
    [ E.button
        {- Another event listener! Fantastic. In this one we do some real work.
        When the button is clicked, we first update the component model to
        reflect that we are in the process of loading a new cat image. Then we
        actually load the cat image, which takes a few seconds. Finally we update
        the model again with the newly-generated cat image. -}
      [ P.onClick \
            _event {- Incoming DOM event, which we ignore -}
            (updateModel :: (CatGeneratorModel -> CatGeneratorModel) -> Effect Unit)
          -> launchAff_ do
              liftEffect do updateModel (_ { catImage = Loading })
              { html } <- generateCat { width: model.catWidth, height: model.catHeight }
              liftEffect do updateModel (_ { catImage = CatHtml { rawHtml: html } })
        {- Disable the button while generating an image -}
      , P.disabled (model.catImage == Loading)
      ]
      [ E.text "Bring in the kitten!"
      ]
    ]
  , E.div
    [ P.addStyles
      [ S.width (show model.catWidth <> "px")
      , S.height (show model.catHeight <> "px")
      , S.border "2px solid red"
      ]
    ]
      {- Here we display either nothing, a loading spinner, or
      the final cat image depending on the state of the cat image
      generation process -}
    [ case model.catImage of
        NoImage -> mempty  {- Empty `Html` value. -}
        Loading -> centered $ spinner "kitten loading"
        CatHtml { rawHtml } -> E.rawHtml rawHtml
    ]
  ]

{- And that's the end of our main component! What's left is some helper
functions and then the top-level component. -}


{- This is what actually performs the "cat generation" operation. Under the
hood this consists of sending an HTTP request to https://placekitten.com/ -}
generateCat :: { width :: Int, height :: Int } -> Aff { html :: String }
generateCat = generateCat_foreign >>> toAffE

foreign import generateCat_foreign ::
  { width :: Int, height :: Int }
  -> Effect (Promise { html :: String })


{- This is a combinator which centers some `Html`, given that the
input `Html` lies within a fixed-width and fixed-height parent. -}
centered :: forall m s. Html m s -> Html m s
centered child =
  E.div
  [ P.addStyles
    [ S.width "100%"
    , S.height "100%"
    , S.display "flex"
    , S.alignItems "center"
    , S.justifyContent "center"
    ]
  ]
  [ child
  ]

{- Turns some text into a fancy animated spinner. The animation is stateless:
it works purely via CSS animation -}
spinner :: forall m s. String -> Html m s
spinner spinnerText =
  E.span
  [ P.addStyles
    [ S.display "inline-block"
    ]
  ]
    {- Here we use `foldMapWithIndex` to iterate over the spinner text
    letter-by-letter, generate some `Html` for each letter, and combine
    all the results. This works because `Html` is a monoid. Very nifty! -}
  [ spinnerText # String.toCharArray # foldMapWithIndex \idx letter ->
      E.span
      [ P.addStyles
        [ bouncy idx
        ]
      ]
      [ E.text (String.singleton letter)
      ]
  ]

  where

  bouncy :: Int -> Style
  bouncy idx = fold
    [ S.position "relative"
    {- Here, `withPrelude` is used to define some "setup CSS" for a style. -}
    , S.withPrelude (intercalate "\n"
      [ "@keyframes bounce {"
      , " 0%   { bottom:  0.25em; }"
      , " 50%  { bottom: -0.25em; }"
      , " 100% { bottom:  0.25em; }"
      , "}"
      ])
      (let animationLength = 1.0
           delay = 0.075 * Int.toNumber idx
       in S.animation $ intercalate " "
         [ "bounce "
         , show animationLength <> "s"
         , "ease-in-out"
         , show (delay - animationLength) <> "s"
         , "infinite"
         ])
    ]


{- Our final component is the top-level component. This will just be a
thin wrapper around the previous component.

Accordingly, its model type is inherited: -}
type Model = CatGeneratorModel

{- Initial model value for the top-level comopnent -}
initial :: Model
initial =
  { catWidth: 400
  , catHeight: 300
  , catImage: NoImage
  }

{- Top-level render function -}
render :: Model -> Html' Model
render model =
  E.div
  [ P.addStyles
    [ S.fontFamily "sans-serif"
      {- Mation has built-in support for complex CSS selectors. This can
      come in handy when writing blanket styles or when tweaking performance.
      Here we create a `Style` value which corresponds to the CSS

        NODE button:hover,
        NODE input[type=range]:hover
        {
          cursor: pointer;
        }

      where NODE selects the rendered node. -}
    , S.on (
        (Sel.descendantsWhere "button" #<> Sel.descendantsWhere "input[type=range]")
        #>> Sel.hover
      )
      [ S.cursor "pointer"
      ]
    ]
  ]
    {- Embed the cat generator component. Since its model type is the same as
    the model type of the top-level component, we don't have to `enroot`. -}
  [ renderCatGenerator model
  ]


{- Run the app! -}
main :: Effect Unit
main = runApp
  { initial
  , render
  , root: onBody  {- Mount to the <body> node -}
  , daemon: mempty  {- We don't use this :p -}
  }

{- Note: if you're editing this file you might notice that changing `main`
doesn't actually do anything. Indeed, this `main` is never actually called,
due to technicalities of how the Mation samples/ folder is set up.
The `main` we define here is just an example of what a `main` would look
like in an actual application. -}

{- Ignore these hehe -}
derive instance Generic CatImage _
instance UnsureEq CatImage where unsureEq = genericUnsureEq

