module Mation.Samples.Main where

import Prelude

import Effect (Effect)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (class Foldable, find, fold, foldMap)
import Data.Lens.Setter ((.~))
import Data.Functor.Contravariant (cmap)
import Data.Enum (class Enum, enumFromTo)
import Data.Monoid (guard)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Enum.Generic (genericSucc, genericPred)
import Data.Bounded.Generic (genericTop, genericBottom)

import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Selectors as Sel
import Mation.Core.Refs as Refs
import Mation.Lenses (field)


data Sample
  = Welcome
  | Counter
  | AsyncApiCall
  | Clock
  | Styling

derive instance Generic Sample _
derive instance Eq Sample
derive instance Ord Sample
instance Show Sample where show = genericShow
instance Enum Sample where
  succ = genericSucc
  pred = genericPred
instance Bounded Sample where
  top = genericTop
  bottom = genericBottom

display :: Sample -> String
display = show

getHref :: Sample -> String
getHref sample = show sample <> ".html"

allSamples :: Array Sample
allSamples = enumFromTo bottom top


type Model =
  { selectedSample :: Sample
  }

initial :: Model
initial =
  { selectedSample: Welcome
  }


daemon :: Refs.ReadWriteL Model -> Effect Unit
daemon ref = do

  -- Load sample from URL, if applicable
  do
    sampleName <- getUrlHash
    let mSample = allSamples # find (\s -> display s == sampleName)
    mSample # foldMap \sample -> ref # Refs.modify (field @"selectedSample" .~ sample)

  -- When model changes, save selected sample to URL
  ref # Refs.onChange \model ->
    setUrlHash (display model.selectedSample)

foreign import setUrlHash :: String -> Effect Unit
foreign import getUrlHash :: Effect String


sampleGroups :: Array (String /\ Array Sample)
sampleGroups =
  [ "Welcome" /\ [ Welcome ]
  , "Basic"   /\ [ Counter, AsyncApiCall, Clock, Styling ]
  ]

render :: Model -> E.Html' (M.Modify Model)
render model =
  E.body
  [ P.addStyles
    [ S.margin "0"
    , S.height "100vh"

    , S.display "grid"
    , S.gridTemplateAreas "'sidebar iframe'"
    , S.gridTemplateColumns "400px 1fr"

    , S.fontFamily "sans-serif"
    ]
  ]

  [ E.div
    [ P.remark "sidebar"
    , P.addStyles
      [ S.gridArea "sidebar"
      , S.background "rgba(0, 0, 0, 0.1)"

      , S.display "flex"
      , S.flexDirection "column"
      , S.alignItems "stretch"
      , S.justifyContent "flex-start"

      , S.on Sel.children
        [ S.padding "0.65rem 1.5rem"
        ]
      ]
    ]
    [ E.h1
      [ P.remark "page title"
      , P.addStyles
        [ S.fontSize "1.75em"
        , S.margin "0"
        , S.marginTop "0.5em"
        , S.paddingBottom "0"
        ]
      ]
      [ E.text "Mation Samples"
      ]

    , sampleGroups # foldMap \(groupName /\ samples) ->
      fold
      [ E.h2
        [ P.addStyles
          [ S.fontSize "1.1em"
          , S.fontWeight "bold"
          , S.margin "0"
          , S.marginTop "1em"
          ]
        ]
        [ E.text groupName ]
      , samples # foldMap \sample ->
          let isSelected = sample == model.selectedSample
          in
          E.a
          [ P.addStyles
            [ S.cursor "pointer"
            , (S.on Sel.hover <> foldGuard isSelected)
              [ S.background "rgba(0, 0, 0, 0.1)"
              ]
            ]
          , P.onClick \_ -> Refs.modify (field @"selectedSample" .~ sample)
          ]
          [ E.text (display sample) ]
      ]
    ]

  , E.iframe
    [ P.remark "iframe"
    , P.addStyles
      [ S.gridArea "iframe"
      , S.width "100%"
      , S.height "100%"
      , S.border "none"
      ]
    , P.src $ getHref model.selectedSample
    ]
    []
  ]

  where

  foldGuard :: forall f a. Foldable f => Monoid a => Boolean -> f a -> a
  foldGuard b = guard b fold



main :: Effect Unit
main = do
  M.runApp
    { initial
    , render: render >>> cmap Refs.downcast
    , root: M.onBody
    , daemon
    }
