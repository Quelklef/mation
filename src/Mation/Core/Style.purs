module Mation.Core.Style where

import Mation.Core.Prelude
import Mation.Core.Html (Prop, mkPair)
import Mation.Core.Many (class Many, float)


-- | A single style
data Style1 = Style1 String String

-- | Html styles
-- |
-- | This is the free monoid over `Style1`
-- |
-- | Since this type instantiates `Monoid`, it can be used with functions like `when` and `foldMap`.
-- | This can be very handy when constructing `Html` values!
newtype Style = Style (Array Style1)

instance Many Style Style1

derive instance Newtype Style _
derive newtype instance Semigroup Style
derive newtype instance Monoid Style


-- | Create a style
mkStyle :: String -> String -> Style
mkStyle k v = Style [ Style1 k v ]


-- | Convert a style array to a `Prop`
toProp :: forall m s. Array Style -> Prop m s
toProp =
  float
  >>> map (\(Style1 k v) -> k <> ": " <> v)
  >>> intercalate "; "
  >>> mkPair "style"
