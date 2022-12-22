
-- | Functions dealing with `Style` values

module Mation.Styles (module X, Scope (..), on) where
 
import Mation.Gen.Styles as X

import Mation.Core.Prelude

import Mation.Core.Style as S
import Mation.Core.Util.PuncturedFold as PF


-- | Represents a "style scope", which is a piece of logic dictating
-- | to whom some styling applies. Style scopes are one of:
-- |
-- | - block-level scopes, like `@media (max-width: 500px)` or `@print`
-- | - selector-level scopes, like `:empty` or `> p:second-child`
-- |
-- | Construct scopes in `Mation.Selectors`
-- |
-- | Note: selector-level scopes and selectors are not the same
-- | thing! A selector acts to produce a collection of elements
-- | from the DOM. A selector *scope*, on the other hand, modifies
-- | a selector.
newtype Scope = Scope (S.StyleScope PF.PuncturedFold)

-- | Apply a style scope to some styles
-- |
-- | For instance,
-- |
-- | ```
-- | import Mation.Styles as S
-- | import Mation.Selectors as Sel
-- | S.on (Sel.children >> Sel.hover) [ S.color "red" ]
-- | ```
-- |
-- | will apply the `color: red` style to hovered children
on :: Scope -> Array S.Style -> S.Style
on (Scope sco) = S.mkScoped sco <<< fold

