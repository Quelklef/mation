module Mation.Elems (module X, module Mation.Elems) where

import Mation.Core.Html (Html, embed) as X
import Mation.Gen.Tags as X

import Effect (Effect)

import Mation.Core.Html (Html (..), mkText)

type Html' s = Html Effect s

text :: forall m s. String -> Html m s
text = mkText
