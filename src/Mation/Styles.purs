
-- | Functions dealing with `Style` values

module Mation.Styles (module X, module Mation.Styles) where
 
import Mation.Gen.Styles as X

import Mation.Core.Prelude

import Mation.Core.Style as S
import Mation.Core.Util.PuncturedFold as PF
import Mation.Core.Util.FreeMonoid as FM


-- | Represents a style scope
-- |
-- | This includes both block-level scopes like `@media` rules
-- | as well as selector-level scopes like `:empty`
-- |
-- | Note that selector-level scopes and selectors are not the same
-- | thing! A selector acts to produce a collection of elements
-- | from the DOM. A selector *scope*, on the other hand, modifies
-- | a selector.
newtype Scope = Scope (S.StyleScope PF.PuncturedFold)

on :: Scope -> Array S.Style -> S.Style
on (Scope sco) = S.mkScoped sco <<< fold

