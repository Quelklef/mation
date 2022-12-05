module Mation.Core.Style where

import Mation.Core.Prelude
import Mation.Core.Html (Prop, mkPair)
import Mation.Core.Many (class Many, float)


data Style1 = Style1 String String

newtype Style = Style (Array Style1)

instance Many Style Style1

derive instance Newtype Style _
derive newtype instance Semigroup Style
derive newtype instance Monoid Style


mkStyle :: String -> String -> Style
mkStyle k v = Style [ Style1 k v ]


toProp :: forall m s. Array Style -> Prop m s
toProp =
  float
  >>> map (\(Style1 k v) -> k <> ": " <> v)
  >>> intercalate "; "
  >>> mkPair "style"
