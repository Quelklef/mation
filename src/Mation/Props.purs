module Mation.Props (module X, style', onInput') where

import Mation.Core.Html (Prop) as X
import Mation.Gen.Attributes as X
import Mation.Gen.Events as X

import Mation.Core.Prelude
import Mation.Core.Mation (Mation)
import Mation.Core.Html (Prop, mkPair)
import Mation.Core.Dom (DomEvent)
import Mation.Core.Style (Style (..))
import Mation.Core.Style as Style

style' :: forall m s. Array Style -> Prop m s
style' = Style.toProp

onInput' :: forall m s. (String -> Mation m s) -> Prop m s
onInput' f = X.onInput (\ev -> f (getTargetValue ev))

foreign import getTargetValue :: DomEvent -> String
