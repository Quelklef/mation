-- | This module contains generated code relating to CSS pseudo-classes.
-- |
-- | Names are converted from their canonical `kebab-case` form into `camelCase`, which is idiomatic for Purescript. For example, `align-content` becomes `alignContent`. Purescript-reserved words are suffixed by an understore, so `type` becomes `type_`.

module Mation.Gen.PseudoClasses where

import Mation.Core.Prelude

import Mation.Core.Style (Style)
import Mation.Core.Style as S
import Mation.Core.Util.FreeMonoid as FM
import Mation.Core.Util.PuncturedFold as PF


-- | [CSS :active pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:active). This is generated code.
onActive ::  Array Style -> Style
onActive  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":active" ]) (S.SConcat $ FM.float styles)

-- | [CSS :any-link pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:any-link). This is generated code.
onAnyLink ::  Array Style -> Style
onAnyLink  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":any-link" ]) (S.SConcat $ FM.float styles)

-- | [CSS :autofill pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:autofill). This is generated code.
onAutofill ::  Array Style -> Style
onAutofill  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":autofill" ]) (S.SConcat $ FM.float styles)

-- | [CSS :checked pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:checked). This is generated code.
onChecked ::  Array Style -> Style
onChecked  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":checked" ]) (S.SConcat $ FM.float styles)

-- | [CSS :default pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:default). This is generated code.
onDefault ::  Array Style -> Style
onDefault  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":default" ]) (S.SConcat $ FM.float styles)

-- | [CSS :defined pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:defined). This is generated code.
onDefined ::  Array Style -> Style
onDefined  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":defined" ]) (S.SConcat $ FM.float styles)

-- | [CSS :disabled pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:disabled). This is generated code.
onDisabled ::  Array Style -> Style
onDisabled  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":disabled" ]) (S.SConcat $ FM.float styles)

-- | [CSS :empty pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:empty). This is generated code.
onEmpty ::  Array Style -> Style
onEmpty  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":empty" ]) (S.SConcat $ FM.float styles)

-- | [CSS :enabled pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:enabled). This is generated code.
onEnabled ::  Array Style -> Style
onEnabled  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":enabled" ]) (S.SConcat $ FM.float styles)

-- | [CSS :first pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:first). This is generated code.
onFirst ::  Array Style -> Style
onFirst  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":first" ]) (S.SConcat $ FM.float styles)

-- | [CSS :first-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:first-child). This is generated code.
onFirstChild ::  Array Style -> Style
onFirstChild  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":first-child" ]) (S.SConcat $ FM.float styles)

-- | [CSS :first-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:first-of-type). This is generated code.
onFirstOfType ::  Array Style -> Style
onFirstOfType  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":first-of-type" ]) (S.SConcat $ FM.float styles)

-- | [CSS :fullscreen pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:fullscreen). This is generated code.
onFullscreen ::  Array Style -> Style
onFullscreen  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":fullscreen" ]) (S.SConcat $ FM.float styles)

-- | [CSS :focus pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:focus). This is generated code.
onFocus ::  Array Style -> Style
onFocus  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":focus" ]) (S.SConcat $ FM.float styles)

-- | [CSS :focus-visible pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:focus-visible). This is generated code.
onFocusVisible ::  Array Style -> Style
onFocusVisible  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":focus-visible" ]) (S.SConcat $ FM.float styles)

-- | [CSS :focus-within pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:focus-within). This is generated code.
onFocusWithin ::  Array Style -> Style
onFocusWithin  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":focus-within" ]) (S.SConcat $ FM.float styles)

-- | [CSS :host pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:host). This is generated code.
onHost ::  Array Style -> Style
onHost  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":host" ]) (S.SConcat $ FM.float styles)

-- | [CSS :hover pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:hover). This is generated code.
onHover ::  Array Style -> Style
onHover  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":hover" ]) (S.SConcat $ FM.float styles)

-- | [CSS :indeterminate pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:indeterminate). This is generated code.
onIndeterminate ::  Array Style -> Style
onIndeterminate  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":indeterminate" ]) (S.SConcat $ FM.float styles)

-- | [CSS :in-range pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:in-range). This is generated code.
onInRange ::  Array Style -> Style
onInRange  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":in-range" ]) (S.SConcat $ FM.float styles)

-- | [CSS :invalid pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:invalid). This is generated code.
onInvalid ::  Array Style -> Style
onInvalid  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":invalid" ]) (S.SConcat $ FM.float styles)

-- | [CSS :is pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:is). This is generated code.
onIs :: String ->  Array Style -> Style
onIs x1 styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":is(" <> x1 <> ")" ]) (S.SConcat $ FM.float styles)

-- | [CSS :lang pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:lang). This is generated code.
onLang :: String ->  Array Style -> Style
onLang x1 styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":lang(" <> x1 <> ")" ]) (S.SConcat $ FM.float styles)

-- | [CSS :last-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:last-child). This is generated code.
onLastChild ::  Array Style -> Style
onLastChild  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":last-child" ]) (S.SConcat $ FM.float styles)

-- | [CSS :last-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:last-of-type). This is generated code.
onLastOfType ::  Array Style -> Style
onLastOfType  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":last-of-type" ]) (S.SConcat $ FM.float styles)

-- | [CSS :left pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:left). This is generated code.
onLeft ::  Array Style -> Style
onLeft  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":left" ]) (S.SConcat $ FM.float styles)

-- | [CSS :link pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:link). This is generated code.
onLink ::  Array Style -> Style
onLink  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":link" ]) (S.SConcat $ FM.float styles)

-- | [CSS :modal pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:modal). This is generated code.
onModal ::  Array Style -> Style
onModal  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":modal" ]) (S.SConcat $ FM.float styles)

-- | [CSS :not pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:not). This is generated code.
onNot :: String ->  Array Style -> Style
onNot x1 styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":not(" <> x1 <> ")" ]) (S.SConcat $ FM.float styles)

-- | [CSS :nth-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-child). This is generated code.
onNthChild :: String ->  Array Style -> Style
onNthChild x1 styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":nth-child(" <> x1 <> ")" ]) (S.SConcat $ FM.float styles)

-- | [CSS :nth-last-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-last-child). This is generated code.
onNthLastChild :: String ->  Array Style -> Style
onNthLastChild x1 styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":nth-last-child(" <> x1 <> ")" ]) (S.SConcat $ FM.float styles)

-- | [CSS :nth-last-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-last-of-type). This is generated code.
onNthLastOfType :: String ->  Array Style -> Style
onNthLastOfType x1 styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":nth-last-of-type(" <> x1 <> ")" ]) (S.SConcat $ FM.float styles)

-- | [CSS :nth-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-of-type). This is generated code.
onNthOfType :: String ->  Array Style -> Style
onNthOfType x1 styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":nth-of-type(" <> x1 <> ")" ]) (S.SConcat $ FM.float styles)

-- | [CSS :only-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:only-child). This is generated code.
onOnlyChild ::  Array Style -> Style
onOnlyChild  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":only-child" ]) (S.SConcat $ FM.float styles)

-- | [CSS :only-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:only-of-type). This is generated code.
onOnlyOfType ::  Array Style -> Style
onOnlyOfType  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":only-of-type" ]) (S.SConcat $ FM.float styles)

-- | [CSS :optional pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:optional). This is generated code.
onOptional ::  Array Style -> Style
onOptional  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":optional" ]) (S.SConcat $ FM.float styles)

-- | [CSS :out-of-range pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:out-of-range). This is generated code.
onOutOfRange ::  Array Style -> Style
onOutOfRange  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":out-of-range" ]) (S.SConcat $ FM.float styles)

-- | [CSS :picture-in-picture pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:picture-in-picture). This is generated code.
onPictureInPicture ::  Array Style -> Style
onPictureInPicture  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":picture-in-picture" ]) (S.SConcat $ FM.float styles)

-- | [CSS :placeholder-shown pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:placeholder-shown). This is generated code.
onPlaceholderShown ::  Array Style -> Style
onPlaceholderShown  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":placeholder-shown" ]) (S.SConcat $ FM.float styles)

-- | [CSS :paused pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:paused). This is generated code.
onPaused ::  Array Style -> Style
onPaused  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":paused" ]) (S.SConcat $ FM.float styles)

-- | [CSS :playing pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:playing). This is generated code.
onPlaying ::  Array Style -> Style
onPlaying  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":playing" ]) (S.SConcat $ FM.float styles)

-- | [CSS :read-only pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:read-only). This is generated code.
onReadOnly ::  Array Style -> Style
onReadOnly  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":read-only" ]) (S.SConcat $ FM.float styles)

-- | [CSS :read-write pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:read-write). This is generated code.
onReadWrite ::  Array Style -> Style
onReadWrite  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":read-write" ]) (S.SConcat $ FM.float styles)

-- | [CSS :required pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:required). This is generated code.
onRequired ::  Array Style -> Style
onRequired  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":required" ]) (S.SConcat $ FM.float styles)

-- | [CSS :right pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:right). This is generated code.
onRight ::  Array Style -> Style
onRight  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":right" ]) (S.SConcat $ FM.float styles)

-- | [CSS :root pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:root). This is generated code.
onRoot ::  Array Style -> Style
onRoot  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":root" ]) (S.SConcat $ FM.float styles)

-- | [CSS :scope pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:scope). This is generated code.
onScope ::  Array Style -> Style
onScope  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":scope" ]) (S.SConcat $ FM.float styles)

-- | [CSS :target pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:target). This is generated code.
onTarget ::  Array Style -> Style
onTarget  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":target" ]) (S.SConcat $ FM.float styles)

-- | [CSS :valid pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:valid). This is generated code.
onValid ::  Array Style -> Style
onValid  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":valid" ]) (S.SConcat $ FM.float styles)

-- | [CSS :visited pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:visited). This is generated code.
onVisited ::  Array Style -> Style
onVisited  styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":visited" ]) (S.SConcat $ FM.float styles)

-- | [CSS :where pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:where). This is generated code.
onWhere :: String ->  Array Style -> Style
onWhere x1 styles = FM.singleton $ S.SScopeASelector (PF.PF [ PF.Hole, PF.Elem $ ":where(" <> x1 <> ")" ]) (S.SConcat $ FM.float styles)

