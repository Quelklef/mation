
-- | Functions dealing with `Style` values

module Mation.Styles (module Mation.Styles, module X) where
 
import Mation.Gen.Styles as X
import Mation.Core.Style (Style) as X
import Mation.Core.StyleScopeModifier (on) as X

import Mation.Core.Style (Style)
import Mation.Core.Style as Style
import Mation.Core.Util.FreeMonoid as FM


-- | Construct a style directly from a key/value pair
-- |
-- | Generally should not be used
mkPair :: String -> String -> Style
mkPair = Style.mkPair

-- | Add some "setup" CSS to a style, such as an animation definition
-- |
-- | Example:
-- | ```
-- | import Mation.Styles as S
-- |
-- | S.withPrelude
-- |   "@keyframes my-anim { 0% { color: red; } 100% { color: blue } }"
-- |   S.animation "my-anim"
-- | ```
-- |
-- | The setup CSS is *global*, meaning two things:
-- |
-- | 1. It is *your* responsibility to prevent name clashes.
-- |
-- |    (Sorry, I wanted the framework to be able to provide some
-- |    help here, but it turns out to be a tough problem)
-- |
-- | 2. This function *can* be used to inject global styles into
-- |    the page... But if you're gonna do that then you should
-- |    probably just use a `<style>` element anyway!
withPrelude :: String -> Style -> Style
withPrelude p = FM.map (Style.addPrelude p)

