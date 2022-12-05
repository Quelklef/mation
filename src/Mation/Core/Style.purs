module Mation.Core.Style where

import Mation.Core.Prelude
import Mation.Core.Html (Prop, mkPair)


data Style1 = Style1 String String

newtype Style = Style (Array Style1)

instance Semigroup Style where
  append (Style a) (Style b) = Style (a <> b)

instance Monoid Style where
  mempty = Style []


mkStyle :: String -> String -> Style
mkStyle k v = Style [ Style1 k v ]


toProp :: forall m s. Array Style -> Prop m s
toProp =

  float
  >>> map (\(Style1 k v) -> k <> ": " <> v)
  >>> intercalate "; "
  >>> mkPair "style"

  where

  float :: Array Style -> Array Style1
  float arr = arr >>= \(Style arr') -> arr'
