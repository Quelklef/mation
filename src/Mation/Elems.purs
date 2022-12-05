module Mation.Elems (module X, module Mation.Elems) where

import Mation.Core.Html (Html, Html', embed) as X
import Mation.Gen.Tags as X

import Mation.Core.Html (Html (..))

text :: forall m s. String -> Html m s
text = HText
