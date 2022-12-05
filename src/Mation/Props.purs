module Mation.Props (module X, module Mation.Props) where

import Mation.Core.Html (Prop) as X
import Mation.Gen.Attributes as X
import Mation.Gen.Events as X

import Mation.Core.Prelude
import Mation.Core.Mation (Mation)
import Mation.Core.Html (Prop, mkPair, DOMEvent)

style' :: forall m s. Array (String /\ String) -> Prop m s
style' = map (\(k /\ v) -> k <> ": " <> v) >>> intercalate "; " >>> mkPair "style"

onInput' :: forall m s. (String -> Mation m s) -> Prop m s
onInput' f = X.onInput (\ev -> f (getTargetValue ev))

foreign import getTargetValue :: DOMEvent -> String
