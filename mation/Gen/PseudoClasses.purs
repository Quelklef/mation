-- | This module contains generated code relating to CSS pseudo-classes.
-- |
-- | Names are converted from their canonical `kebab-case` form into `camelCase`, which is idiomatic for Purescript. For example, `align-content` becomes `alignContent`. Purescript-reserved words are suffixed by an understore, so `type` becomes `type_`.

module Mation.Gen.PseudoClasses where

import Mation.Core.Prelude

import Mation.Core.StyleScopeModifier (ScopeModifier (..))
import Mation.Core.Util.Weave as W


-- | [CSS :active pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:active). This is generated code.
active :: ScopeModifier
active = SMAlts [ { selector: W.that <> W.this (":active"), block: W.noop } ]

-- | [CSS :any-link pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:any-link). This is generated code.
anyLink :: ScopeModifier
anyLink = SMAlts [ { selector: W.that <> W.this (":any-link"), block: W.noop } ]

-- | [CSS :autofill pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:autofill). This is generated code.
autofill :: ScopeModifier
autofill = SMAlts [ { selector: W.that <> W.this (":autofill"), block: W.noop } ]

-- | [CSS :checked pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:checked). This is generated code.
checked :: ScopeModifier
checked = SMAlts [ { selector: W.that <> W.this (":checked"), block: W.noop } ]

-- | [CSS :default pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:default). This is generated code.
default :: ScopeModifier
default = SMAlts [ { selector: W.that <> W.this (":default"), block: W.noop } ]

-- | [CSS :defined pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:defined). This is generated code.
defined :: ScopeModifier
defined = SMAlts [ { selector: W.that <> W.this (":defined"), block: W.noop } ]

-- | [CSS :disabled pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:disabled). This is generated code.
disabled :: ScopeModifier
disabled = SMAlts [ { selector: W.that <> W.this (":disabled"), block: W.noop } ]

-- | [CSS :empty pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:empty). This is generated code.
empty :: ScopeModifier
empty = SMAlts [ { selector: W.that <> W.this (":empty"), block: W.noop } ]

-- | [CSS :enabled pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:enabled). This is generated code.
enabled :: ScopeModifier
enabled = SMAlts [ { selector: W.that <> W.this (":enabled"), block: W.noop } ]

-- | [CSS :first pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:first). This is generated code.
first :: ScopeModifier
first = SMAlts [ { selector: W.that <> W.this (":first"), block: W.noop } ]

-- | [CSS :first-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:first-child). This is generated code.
firstChild :: ScopeModifier
firstChild = SMAlts [ { selector: W.that <> W.this (":first-child"), block: W.noop } ]

-- | [CSS :first-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:first-of-type). This is generated code.
firstOfType :: ScopeModifier
firstOfType = SMAlts [ { selector: W.that <> W.this (":first-of-type"), block: W.noop } ]

-- | [CSS :fullscreen pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:fullscreen). This is generated code.
fullscreen :: ScopeModifier
fullscreen = SMAlts [ { selector: W.that <> W.this (":fullscreen"), block: W.noop } ]

-- | [CSS :focus pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:focus). This is generated code.
focus :: ScopeModifier
focus = SMAlts [ { selector: W.that <> W.this (":focus"), block: W.noop } ]

-- | [CSS :focus-visible pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:focus-visible). This is generated code.
focusVisible :: ScopeModifier
focusVisible = SMAlts [ { selector: W.that <> W.this (":focus-visible"), block: W.noop } ]

-- | [CSS :focus-within pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:focus-within). This is generated code.
focusWithin :: ScopeModifier
focusWithin = SMAlts [ { selector: W.that <> W.this (":focus-within"), block: W.noop } ]

-- | [CSS :host pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:host). This is generated code.
host :: ScopeModifier
host = SMAlts [ { selector: W.that <> W.this (":host"), block: W.noop } ]

-- | [CSS :hover pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:hover). This is generated code.
hover :: ScopeModifier
hover = SMAlts [ { selector: W.that <> W.this (":hover"), block: W.noop } ]

-- | [CSS :indeterminate pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:indeterminate). This is generated code.
indeterminate :: ScopeModifier
indeterminate = SMAlts [ { selector: W.that <> W.this (":indeterminate"), block: W.noop } ]

-- | [CSS :in-range pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:in-range). This is generated code.
inRange :: ScopeModifier
inRange = SMAlts [ { selector: W.that <> W.this (":in-range"), block: W.noop } ]

-- | [CSS :invalid pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:invalid). This is generated code.
invalid :: ScopeModifier
invalid = SMAlts [ { selector: W.that <> W.this (":invalid"), block: W.noop } ]

-- | [CSS :is pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:is). This is generated code.
is :: String -> ScopeModifier
is x1 = SMAlts [ { selector: W.that <> W.this (":is(" <> x1 <> ")"), block: W.noop } ]

-- | [CSS :lang pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:lang). This is generated code.
lang :: String -> ScopeModifier
lang x1 = SMAlts [ { selector: W.that <> W.this (":lang(" <> x1 <> ")"), block: W.noop } ]

-- | [CSS :last-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:last-child). This is generated code.
lastChild :: ScopeModifier
lastChild = SMAlts [ { selector: W.that <> W.this (":last-child"), block: W.noop } ]

-- | [CSS :last-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:last-of-type). This is generated code.
lastOfType :: ScopeModifier
lastOfType = SMAlts [ { selector: W.that <> W.this (":last-of-type"), block: W.noop } ]

-- | [CSS :left pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:left). This is generated code.
left :: ScopeModifier
left = SMAlts [ { selector: W.that <> W.this (":left"), block: W.noop } ]

-- | [CSS :link pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:link). This is generated code.
link :: ScopeModifier
link = SMAlts [ { selector: W.that <> W.this (":link"), block: W.noop } ]

-- | [CSS :modal pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:modal). This is generated code.
modal :: ScopeModifier
modal = SMAlts [ { selector: W.that <> W.this (":modal"), block: W.noop } ]

-- | [CSS :not pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:not). This is generated code.
not :: String -> ScopeModifier
not x1 = SMAlts [ { selector: W.that <> W.this (":not(" <> x1 <> ")"), block: W.noop } ]

-- | [CSS :nth-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-child). This is generated code.
nthChild :: String -> ScopeModifier
nthChild x1 = SMAlts [ { selector: W.that <> W.this (":nth-child(" <> x1 <> ")"), block: W.noop } ]

-- | [CSS :nth-last-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-last-child). This is generated code.
nthLastChild :: String -> ScopeModifier
nthLastChild x1 = SMAlts [ { selector: W.that <> W.this (":nth-last-child(" <> x1 <> ")"), block: W.noop } ]

-- | [CSS :nth-last-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-last-of-type). This is generated code.
nthLastOfType :: String -> ScopeModifier
nthLastOfType x1 = SMAlts [ { selector: W.that <> W.this (":nth-last-of-type(" <> x1 <> ")"), block: W.noop } ]

-- | [CSS :nth-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-of-type). This is generated code.
nthOfType :: String -> ScopeModifier
nthOfType x1 = SMAlts [ { selector: W.that <> W.this (":nth-of-type(" <> x1 <> ")"), block: W.noop } ]

-- | [CSS :only-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:only-child). This is generated code.
onlyChild :: ScopeModifier
onlyChild = SMAlts [ { selector: W.that <> W.this (":only-child"), block: W.noop } ]

-- | [CSS :only-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:only-of-type). This is generated code.
onlyOfType :: ScopeModifier
onlyOfType = SMAlts [ { selector: W.that <> W.this (":only-of-type"), block: W.noop } ]

-- | [CSS :optional pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:optional). This is generated code.
optional :: ScopeModifier
optional = SMAlts [ { selector: W.that <> W.this (":optional"), block: W.noop } ]

-- | [CSS :out-of-range pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:out-of-range). This is generated code.
outOfRange :: ScopeModifier
outOfRange = SMAlts [ { selector: W.that <> W.this (":out-of-range"), block: W.noop } ]

-- | [CSS :picture-in-picture pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:picture-in-picture). This is generated code.
pictureInPicture :: ScopeModifier
pictureInPicture = SMAlts [ { selector: W.that <> W.this (":picture-in-picture"), block: W.noop } ]

-- | [CSS :placeholder-shown pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:placeholder-shown). This is generated code.
placeholderShown :: ScopeModifier
placeholderShown = SMAlts [ { selector: W.that <> W.this (":placeholder-shown"), block: W.noop } ]

-- | [CSS :paused pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:paused). This is generated code.
paused :: ScopeModifier
paused = SMAlts [ { selector: W.that <> W.this (":paused"), block: W.noop } ]

-- | [CSS :playing pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:playing). This is generated code.
playing :: ScopeModifier
playing = SMAlts [ { selector: W.that <> W.this (":playing"), block: W.noop } ]

-- | [CSS :read-only pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:read-only). This is generated code.
readOnly :: ScopeModifier
readOnly = SMAlts [ { selector: W.that <> W.this (":read-only"), block: W.noop } ]

-- | [CSS :read-write pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:read-write). This is generated code.
readWrite :: ScopeModifier
readWrite = SMAlts [ { selector: W.that <> W.this (":read-write"), block: W.noop } ]

-- | [CSS :required pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:required). This is generated code.
required :: ScopeModifier
required = SMAlts [ { selector: W.that <> W.this (":required"), block: W.noop } ]

-- | [CSS :right pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:right). This is generated code.
right :: ScopeModifier
right = SMAlts [ { selector: W.that <> W.this (":right"), block: W.noop } ]

-- | [CSS :root pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:root). This is generated code.
root :: ScopeModifier
root = SMAlts [ { selector: W.that <> W.this (":root"), block: W.noop } ]

-- | [CSS :scope pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:scope). This is generated code.
scope :: ScopeModifier
scope = SMAlts [ { selector: W.that <> W.this (":scope"), block: W.noop } ]

-- | [CSS :target pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:target). This is generated code.
target :: ScopeModifier
target = SMAlts [ { selector: W.that <> W.this (":target"), block: W.noop } ]

-- | [CSS :valid pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:valid). This is generated code.
valid :: ScopeModifier
valid = SMAlts [ { selector: W.that <> W.this (":valid"), block: W.noop } ]

-- | [CSS :visited pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:visited). This is generated code.
visited :: ScopeModifier
visited = SMAlts [ { selector: W.that <> W.this (":visited"), block: W.noop } ]

-- | [CSS :where pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:where). This is generated code.
where_ :: String -> ScopeModifier
where_ x1 = SMAlts [ { selector: W.that <> W.this (":where(" <> x1 <> ")"), block: W.noop } ]

