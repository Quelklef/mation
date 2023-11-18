module Mation.Samples.PerfTest where

{- Sample "application" for testing performance

   Designed to very loosely emulate a chatting
   application in terms of perf. For instance, list items
   are given long content text to match roughly the amount
   of data that might be stored per message in a small
   chatting application.

   -}

import Mation.Core.Prelude

import Data.Array as Array
import Data.Int as Int
import Effect.Console as Console
import Effect.Aff as Aff
import Data.Lens.Index (ix)
import Data.Time.Duration (Milliseconds (..))

import Mation as M
import Mation (Html')
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Selectors as Sel
import Mation.Lenses (field)


foreign import toFixed :: Int -> Number -> String
foreign import getNow :: Effect Number
foreign import timeMe :: Effect Unit -> Effect Number

foreign import calcStats :: Array Number ->
  { mean :: Number
  , median :: Number
  , stdev :: Number
  }


type Model =
  { items :: Array String
  , exp :: Int
  , benchmarks :: Array Number
  , fast :: Boolean
  }


_items :: Lens' Model (Array String)
_items = field @"items"

_exp :: Lens' Model Int
_exp = field @"exp"

_benchmarks :: Lens' Model (Array Number)
_benchmarks = field @"benchmarks"

_fast :: Lens' Model Boolean
_fast = field @"fast"




initial :: Model
initial =
  { exp: 4
  , items: resizeArray' 16 []
  , benchmarks: []
  , fast: false
  }

render :: Model -> E.Html' (M.Modify Model)
render model =
  E.div
  []
  [ E.div
    [ P.addStyles
      [ S.display "flex"
      , S.gap "1em"
      ]
    ]
    [ E.div
      []
      [ E.text "Rendering to 2^"
      , E.input
        [ P.type_ "number"
        , P.onInputValue \val ref ->
              case Int.fromString val of
                Nothing -> pure unit
                Just n -> ref # M.modify ((_exp .~ n) >>> (_items %~ resizeArray' (Int.pow 2 n)))
        , P.value (show model.exp)
        , P.addStyles [ S.width "6ch" ]
        ]
      , E.text $ " = " <> show (Array.length model.items) <> " nodes"
      ]
    , E.text "|"
    , E.div
      []
      [ E.input
        [ P.type_ "checkbox"
        , P.checked model.fast
        , P.onInputValue \_ -> M.modify (_fast %~ not)
        , P.id "use-fast"
        ]
      , E.text " "
      , E.label
        [ P.for "use-fast" ]
        [ E.text "do pruning"
        ]
      ]
    ]
  , E.br []
  , E.div
    []
    [ E.div
      []
      [ E.text "Benchmark: "
      , E.button
        [ P.onClick \_ -> doBench 1
        ]
        [ E.text "one"
        ]
      , E.text " "
      , E.button
        [ P.onClick \_ -> doBench 10
        ]
        [ E.text "ten"
        ]
      , E.text " "
      , E.button
        [ P.onClick \_ -> doBench 150
        ]
        [ E.text "one hundred and fifty"
        ]
      ]
    , guard (Array.length model.benchmarks > 0) $ fold
      [ E.br []
      , E.div
        [ P.addStyles
          [ S.fontFamily "monospace"
          ]
        ]
        [ let stats = calcStats model.benchmarks in
          E.div
          []
          [ E.text $ fold
            [ "Overall: "
            , "mean=" <> toFixed 2 stats.mean <> "ms"
            , " & median=" <> toFixed 2 stats.median <> "ms"
            , " & stdev=" <> toFixed 2 stats.stdev <> "ms"
            ]
          ]
        , E.br []
        , flip foldMap model.benchmarks \ms ->
          E.div
          []
          [ E.text $ fold
              [ "Frame took ≈"
              , toFixed 1 ms
              ,"ms (≈"
              , toFixed 1 (1000.0 / ms)
              , " fps)"
              ]
          ]
        ]
      ]
    ]
  , E.br []
  , E.br []
  , cmap (M.focusWithLens _items) $
      theList model.items
  ]

  where

  theList :: Array String -> Html' (M.Modify (Array String))
  theList items =
    E.div
    [ P.addStyles
      [ S.display "flex"
      , S.flexDirection "column"
      , S.gap ".5em"
      ]
    ]
    [ items
      # foldMapWithIndex \idx str ->
          (cmap (M.focusWithSetter (ix idx))) (listItem idx str)
    ]

  listItem :: Int -> String -> Html' (M.Modify String)
  listItem = \idx str ->
    case model.fast of
      false -> impl idx str
      true -> E.prune ("listitem-" <> show idx) (idx /\ str) \(idx /\ str) -> impl idx str

    where
    impl idx str =
      E.div
      [ P.addStyles
        [ S.border "1px solid grey"
        , S.padding ".35em .75em"
        , S.cursor "pointer"
        , S.on Sel.hover
          [ S.backgroundColor "rgb(255, 220, 255)"
          ]
        , S.display "flex"
        , S.gap ".5em"
        ]
      , P.onClick \_ _ -> Console.log (" item " <> show (idx + 1) <> " clicked")
      , P.remark ("list item " <> show (idx + 1))
      ]
      [ E.div
        [ P.addStyles
          [ S.whiteSpace "nowrap"
          ]
        ]
        [ E.span
          [ P.addStyles
            [ S.fontFamily "monospace"
            , S.textAlign "right"
            , S.fontSize "1.15em"
            , S.width "5ch"
            , S.display "inline-block"
            ]
          ]
          [ E.text (show (idx + 1))
          ]
        , E.text " ~ "
        , E.input
          [ P.value str
          , P.onInputValue \v -> M.write v
          ]
        , E.text " → "
        ]
        , E.div
          []
          [ E.text str
          ]
      ]


resizeArray' :: Int -> Array String -> Array String
resizeArray' = resizeArray \i ->
  [ "item " <> show (i + 1) ]
  # (_ `power` 36)
  # intercalate " • "

resizeArray :: forall a. (Int -> a) -> Int -> Array a -> Array a
resizeArray mkItem size arr =
  (<>)
    (Array.slice 0 size arr)
    (rangeIncExc 0 (size - Array.length arr) # map mkItem)

  where

  rangeIncExc :: Int -> Int -> Array Int
  rangeIncExc lo hi =
    if hi <= lo then []
    else Array.range lo (hi - 1)


doBench :: Int -> M.Modify Model -> Effect Unit
doBench count ref = Aff.launchAff_ do
  liftEffect do ref # M.modify (_benchmarks .~ [])
  for_ (Array.range 1 count) \idx -> do
    t0 <- liftEffect getNow
    liftEffect do ref # M.modify (_items <<< ix 0 .~ ("frame #" <> show idx))
      -- ^ Need to actually modify the DOM or we get wacky results
    tf <- liftEffect getNow
    waitTick
    liftEffect do ref # M.modify (_benchmarks %~ (_ <> [tf - t0]))

  where
  waitTick = Aff.delay (Milliseconds 0.0)


-- | Built-in implementation is a composition of mapWithIndex and foldr, which is not performant
foldMapWithIndex :: forall a r. Monoid r => (Int -> a -> r) -> Array a -> r
foldMapWithIndex = foldMapWithIndex_f append mempty

foreign import foldMapWithIndex_f ::
  forall a r.
  (r -> r -> r) -> r ->
  (Int -> a -> r) -> Array a -> r

