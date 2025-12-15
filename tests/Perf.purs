module Mation.Tests.Perf where

{- METADATA BEGIN

{
  "name": "Perf",
  "desc": "Basic Performance Benchmark",
  "specs": []
}

METADATA END -}


{- Sample "application" for testing performance

   Designed to very loosely emulate a chatting
   application in terms of perf. For instance, list items
   are given long content text to match roughly the amount
   of data that might be stored per message in a small
   chatting application.

   -}

import Mation.Core.Prelude

import Effect.Aff as Aff
import Effect.Console as Console
import Data.Array as Array
import Data.Int as Int
import Data.Lens.Index (ix)
import Data.Time.Duration (Milliseconds (..))
import Control.Lazy (fix)

import Mation as M
import Mation (Html')
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Selectors as Sel
import Mation.Lenses (field)
import Mation.Core.Refs as Refs
import Mation.Core.Refs (ReadWrite)


foreign import toFixed :: Int -> Number -> String
foreign import getNow :: Effect Number
foreign import timeMe :: Effect Unit -> Effect Number

foreign import calcStats :: Array Number ->
  { mean :: Number
  , median :: Number
  , stdev :: Number
  }


type Model =
  { listItems :: Array String
  , exp :: Int
  , trials :: Array Number
  , doPruning :: Boolean
  , cancelCurrentBenchmark :: Maybe (Effect Unit)
  }




initial :: Model
initial =
  { exp: 1
  , listItems: resizeArray' 2 []
  , trials: []
  , doPruning: false
  , cancelCurrentBenchmark: Nothing
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
                Just n -> ref # M.modify ((field @"exp" .~ n) >>> (field @"listItems" %~ resizeArray' (Int.pow 2 n)))
        , P.value (show model.exp)
        , P.addStyles [ S.width "6ch" ]
        ]
      , E.text $ " = " <> show (Array.length model.listItems) <> " nodes"
      ]
    , E.text "|"
    , E.div
      []
      [ E.input
        [ P.type_ "checkbox"
        , P.checked model.doPruning
        , P.onInputValue \_ -> M.modify (field @"doPruning" %~ not)
        , P.id "doPruning"
        ]
      , E.text " "
      , E.label
        [ P.for "doPruning" ]
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
        [ P.onClick \_ -> doBench 1 ]
        [ E.text "x1" ]
      , E.text " "
      , E.button
        [ P.onClick \_ -> doBench 10 ]
        [ E.text "x10" ]
      , E.text " "
      , E.button
        [ P.onClick \_ -> doBench 150 ]
        [ E.text "x150" ]
      , E.text " "
      , E.button
        [ P.onClick \_ -> doBench 1000 ]
        [ E.text "x1000" ]
      , E.text " "
      , E.button
        [ P.onClick \_ -> doBench 5000 ]
        [ E.text "x5000" ]
      , model.cancelCurrentBenchmark # foldMap \cancel ->
        fold
        [ E.text " - "
        , E.button
          [ P.onClick \_ _ -> cancel ]
          [ E.text "CANCEL" ]
        ]
      ]
    , guard (Array.length model.trials > 0) $ fold
      [ E.br []
      , E.div
        [ P.addStyles
          [ S.fontFamily "monospace"
          ]
        ]
        [ let stats = calcStats model.trials in
          E.div
          []
          [ E.text $ fold
            [ "Overall: "
            , "n=" <> show (Array.length model.trials)
            , "; mean=" <> toFixed 2 stats.mean <> "ms"
            , "; median=" <> toFixed 2 stats.median <> "ms"
            , "; stdev=" <> toFixed 2 stats.stdev <> "ms"
            ]
          ]
        , E.br []
        , model.trials # Array.last # foldMap \ms ->
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
  , cmap (M.focusWithLens (field @"listItems")) $
      theList model.listItems
  ]

  where

  theList :: Array String -> Html' (M.Modify (Array String))
  theList listItems =
    E.div
    [ P.addStyles
      [ S.display "flex"
      , S.flexDirection "column"
      , S.gap ".5em"
      ]
    ]
    [ listItems
      # foldMapWithIndex \idx str ->
          (cmap (M.focusWithSetter (ix idx))) (listItem idx str)
    ]

  listItem :: Int -> String -> Html' (M.Modify String)
  listItem = \idx str ->
    case model.doPruning of
      false -> impl idx str
      true -> (idx /\ str) # E.pruneEq ("listitem-" <> show idx) \(idx /\ str) -> impl idx str

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
  (isCancelledRef :: ReadWrite _) <- Refs.make false
  liftEffect do ref # M.modify (
    (field @"trials" .~ [])
    >>> (field @"cancelCurrentBenchmark" .~ Just (isCancelledRef # Refs.write true))
  )

  0 # fix \next idx -> do
    isCancelled <- Refs.read isCancelledRef
    if isCancelled then pure unit
    else if idx >= count then pure mempty
    else do
      t0 <- liftEffect getNow
      liftEffect do ref # M.modify (field @"listItems" <<< ix 0 .~ ("frame #" <> show idx))
        -- ^ Need to actually modify the DOM or we get wacky results
      tf <- liftEffect getNow
      waitTick
      liftEffect do ref # M.modify (field @"trials" %~ (\arr -> Array.snoc arr (tf - t0)))
      next (idx + 1)

  liftEffect do ref # M.modify (field @"cancelCurrentBenchmark" .~ Nothing)

  where
  waitTick = Aff.delay (Milliseconds 0.0)


-- | Built-in implementation is a composition of mapWithIndex and foldr, which is not performant
foldMapWithIndex :: forall a r. Monoid r => (Int -> a -> r) -> Array a -> r
foldMapWithIndex = foldMapWithIndex_f append mempty

foreign import foldMapWithIndex_f ::
  forall a r.
  (r -> r -> r) -> r ->
  (Int -> a -> r) -> Array a -> r


main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }
