module Mation.Tests.PrunedListItems where

{- METADATA START

{
  "name": "Pruned List Items",
  "desc": "Tests lists where every item is pruned (akin to keyed nodes in other frameworks)",
  "specs": [
    "Moving items should work properly",
    "When pruning is enabled, neither moving nor deleting items should cause any items to update"
  ]
}

METADATA END -}

import Mation.Core.Prelude

import Data.Array (range)
import Data.Array as Array
import Data.FoldableWithIndex (foldMapWithIndex)

import Mation as M
import Mation.Elems as E
import Mation.Elems.Prune as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Selectors as Sel
import Mation.Core.Refs (focusWithLens, Modify)
import Mation.Core.Refs as Refs
import Mation.Lenses (field)


type Model =
  { doPruning :: Boolean
  , elems :: Array Elem
  }

type Elem = { id :: Int, content :: String }

mkElem :: Int -> Elem
mkElem id = { id, content: "Elem #" <> show id }

initial :: Model
initial =
  { doPruning: true
  , elems: mkElem <$> range 1 5
  }

render :: Model -> E.Html' (Modify Model)
render model =
  E.div
  [ P.addStyles
    [ S.fontFamily "sans-serif"
    , S.on (Sel.descendantsWhere "button") [ S.fontSize "0.95em" ]
    ]
  ]
  [ E.p
    []
    [ E.label
      []
      [ E.input
        [ P.type_ "checkbox"
        , P.checked model.doPruning
        , P.onClick \_ -> Refs.modify (field @"doPruning" %~ not)
        ]
      , E.text " "
      , E.text "Do pruning"
      ]
    ]
  , E.p
    []
    [ E.button
      [ P.onClick \_ -> Refs.modify (field @"elems" %~ \arr ->
          let maxId = arr # map _.id # maximum # fromMaybe 0
          in Array.snoc arr (mkElem (maxId + 1))
        )
      ]
      [ E.text "➕ New elem" ]
    ]
  , E.p
    []
    [ renderElems model # cmap (focusWithLens (field @"elems"))
    ]
  ]

renderElems :: Model -> E.Html' (Modify (Array Elem))
renderElems model =
  E.ul
  [ P.showUpdates
  ]
  [ model.elems # foldMap \elem ->
      guardEndo model.doPruning
      (E.pruneEq ("elem-" <> show elem.id))
      viewElem
      elem
  ]

  where

  guardEndo b f = if b then f else identity


viewElem :: Elem -> E.Html' (Modify (Array Elem))
viewElem elem =
  E.li
  [ P.addCss "margin: 0.5em 0"
  , P.showUpdates
  ]
  [ E.button [ P.onClick \_ -> moveMe (-1) ] [ E.text "↑" ]
  , E.text " "
  , E.button [ P.onClick \_ -> moveMe 1 ] [ E.text "↓" ]
  , E.text " "
  , E.button [ P.onClick \_ -> deleteMe ] [ E.text "🗑️" ]
  , E.text " "
  , E.text elem.content
  , E.text " "
  , E.text $ "x" `power` elem.id
  ]

  where

  moveMe :: Int -> Modify (Array Elem) -> Effect Unit
  moveMe idxDelta = Refs.modify \arr ->
    fromMaybe arr do
      -- NOTE: We have to re-fetch the elem index because it
      --       may have changed since the elem was rendered!
      currIdx <- Array.findIndex (\e -> e.id == elem.id) arr
      arr' <- Array.deleteAt currIdx arr
      Array.insertAt (currIdx + idxDelta) elem arr'

  deleteMe :: Modify (Array Elem) -> Effect Unit
  deleteMe = Refs.modify \arr ->
    fromMaybe arr do
      currIdx <- Array.findIndex (\e -> e.id == elem.id) arr
      Array.deleteAt currIdx arr



main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.underBody
    , daemon: mempty
    }
