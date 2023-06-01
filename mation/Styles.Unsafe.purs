module Mation.Styles.Unsafe where

import Mation.Core.Style (Style)
import Mation.Core.Style as Style
import Mation.Core.Util.FreeMonoid as FM


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
-- | 1. Name conflicts are possible: if two calls to `withPrelude`
-- |    both make `@keyframes` using the name `my-anim`, then one
-- |    will likely override the other.
-- |
-- |    (Sorry, I wanted the framework to be able to provide some
-- |    help here, but it turns out to be a tough problem)
-- |
-- | 2. This function *can* be used to inject global styles into
-- |    the page, such as adding style to `body`.
-- |    But if you're gonna do that then you should
-- |    probably just use a `<style>` element anyway!
-- |
-- | For these reasons, this operation is considered unsafe.
withPrelude :: String -> Style -> Style
withPrelude p = FM.map (Style.addPrelude p)

