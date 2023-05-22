
-- | Functions dealing with `Style` values

module Mation.Styles (module Mation.Styles, module X) where
 
import Mation.Gen.Styles as X
import Mation.Core.Style (Style) as X
import Mation.Core.StyleScopeModifier (on) as X
import Mation.Styles.Unsafe (withPrelude) as X

import Mation.Core.Style (Style)
import Mation.Core.Style as Style


-- | Construct a style directly from a key/value pair
-- |
-- | This should only rarely be necessary
rawStyle :: String -> String -> Style
rawStyle = Style.mkPair

