
-- | Functions for creating and modifying `Prop`erties

module Mation.Props (module X, style', onInput') where
  
import Mation.Core.Html (Prop) as X
import Mation.Gen.Attributes as X
import Mation.Gen.Events as X

import Mation.Core.Prelude
import Mation.Core.Mation (Mation)
import Mation.Core.Html (Prop, mkPair)
import Mation.Core.Dom (DomEvent)
import Mation.Core.Style (Style (..))
import Mation.Core.Util.PuncturedFold (PuncturedFold)
import Mation.Core.Style as Style

-- | From a collection of `Style` values, produce a `Prop` for the `style` attribute on an `Html` node
style' :: forall m s. Array Style -> Prop m s
style' = Style.toProp

-- | Attach an listener to the `input` event.
-- |
-- | This differs from `onInput` in type: `onInput` provides a `DomEvent`, but this function provides the value `String`
onInput' :: forall m s. (String -> Mation m s) -> Prop m s
onInput' f = X.onInput (\ev -> f (getTargetValue ev))

foreign import getTargetValue :: DomEvent -> String
