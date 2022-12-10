
-- | Functions dealing with `Style` values

module Mation.Styles (module X, module Mation.Styles) where
 
import Mation.Gen.Styles as X
import Mation.Gen.PseudoClasses as X

import Mation.Core.Prelude

import Mation.Core.Style as S
import Mation.Core.Util.PuncturedFold as PF
import Mation.Core.Util.FreeMonoid as FM

onChildren :: Array S.Style -> S.Style
onChildren styles = FM.singleton $
  S.SScopeASelector
    (PF.PF [ PF.Hole, PF.Elem "> *" ])
    (S.SConcat $ FM.float styles)
