module Mation.Props (module X, module Mation.Props) where

import Mation.Core.Html (Prop) as X
import Mation.Gen.Attributes as X
import Mation.Gen.Events as X

import Mation.Core.Prelude
import Mation.Core.Html (Prop (..))

style' :: forall m s. Array (String /\ String) -> Prop m s
style' = map (\(k /\ v) -> k <> ": " <> v) >>> intercalate "; " >>> PPair "style"
