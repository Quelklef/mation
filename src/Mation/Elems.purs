module Mation.Elems (module X, module Mation.Elems) where

import Mation.Core.Html (Html, embed) as X
import Mation.Gen.Tags as X

import Effect (Effect)

import Mation.Core.Html (Html (..))
import Mation.Core.Html as Html

type Html' s = Html Effect s

text :: forall m s. String -> Html m s
text = Html.mkText

rawHtml :: forall m s. String -> Html m s
rawHtml = Html.mkRawHtml
