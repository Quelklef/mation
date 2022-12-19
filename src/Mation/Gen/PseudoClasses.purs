-- | This module contains generated code relating to CSS pseudo-classes.
-- |
-- | Names are converted from their canonical `kebab-case` form into `camelCase`, which is idiomatic for Purescript. For example, `align-content` becomes `alignContent`. Purescript-reserved words are suffixed by an understore, so `type` becomes `type_`.

module Mation.Gen.PseudoClasses where

import Mation.Core.Prelude

import Mation.Core.Style (mkSelectorScope)
import Mation.Styles (Scope (..))
import Mation.Core.Util.FreeMonoid as FM
import Mation.Core.Util.PuncturedFold as PF


-- | [CSS :active pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:active). This is generated code.
active :: Scope
active = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":active" ]

-- | [CSS :any-link pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:any-link). This is generated code.
anyLink :: Scope
anyLink = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":any-link" ]

-- | [CSS :autofill pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:autofill). This is generated code.
autofill :: Scope
autofill = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":autofill" ]

-- | [CSS :checked pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:checked). This is generated code.
checked :: Scope
checked = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":checked" ]

-- | [CSS :default pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:default). This is generated code.
default :: Scope
default = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":default" ]

-- | [CSS :defined pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:defined). This is generated code.
defined :: Scope
defined = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":defined" ]

-- | [CSS :disabled pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:disabled). This is generated code.
disabled :: Scope
disabled = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":disabled" ]

-- | [CSS :empty pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:empty). This is generated code.
empty :: Scope
empty = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":empty" ]

-- | [CSS :enabled pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:enabled). This is generated code.
enabled :: Scope
enabled = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":enabled" ]

-- | [CSS :first pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:first). This is generated code.
first :: Scope
first = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":first" ]

-- | [CSS :first-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:first-child). This is generated code.
firstChild :: Scope
firstChild = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":first-child" ]

-- | [CSS :first-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:first-of-type). This is generated code.
firstOfType :: Scope
firstOfType = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":first-of-type" ]

-- | [CSS :fullscreen pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:fullscreen). This is generated code.
fullscreen :: Scope
fullscreen = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":fullscreen" ]

-- | [CSS :focus pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:focus). This is generated code.
focus :: Scope
focus = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":focus" ]

-- | [CSS :focus-visible pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:focus-visible). This is generated code.
focusVisible :: Scope
focusVisible = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":focus-visible" ]

-- | [CSS :focus-within pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:focus-within). This is generated code.
focusWithin :: Scope
focusWithin = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":focus-within" ]

-- | [CSS :host pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:host). This is generated code.
host :: Scope
host = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":host" ]

-- | [CSS :hover pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:hover). This is generated code.
hover :: Scope
hover = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":hover" ]

-- | [CSS :indeterminate pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:indeterminate). This is generated code.
indeterminate :: Scope
indeterminate = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":indeterminate" ]

-- | [CSS :in-range pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:in-range). This is generated code.
inRange :: Scope
inRange = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":in-range" ]

-- | [CSS :invalid pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:invalid). This is generated code.
invalid :: Scope
invalid = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":invalid" ]

-- | [CSS :is pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:is). This is generated code.
is :: String -> Scope
is x1 = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":is(" <> x1 <> ")" ]

-- | [CSS :lang pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:lang). This is generated code.
lang :: String -> Scope
lang x1 = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":lang(" <> x1 <> ")" ]

-- | [CSS :last-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:last-child). This is generated code.
lastChild :: Scope
lastChild = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":last-child" ]

-- | [CSS :last-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:last-of-type). This is generated code.
lastOfType :: Scope
lastOfType = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":last-of-type" ]

-- | [CSS :left pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:left). This is generated code.
left :: Scope
left = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":left" ]

-- | [CSS :link pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:link). This is generated code.
link :: Scope
link = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":link" ]

-- | [CSS :modal pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:modal). This is generated code.
modal :: Scope
modal = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":modal" ]

-- | [CSS :not pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:not). This is generated code.
not :: String -> Scope
not x1 = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":not(" <> x1 <> ")" ]

-- | [CSS :nth-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-child). This is generated code.
nthChild :: String -> Scope
nthChild x1 = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":nth-child(" <> x1 <> ")" ]

-- | [CSS :nth-last-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-last-child). This is generated code.
nthLastChild :: String -> Scope
nthLastChild x1 = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":nth-last-child(" <> x1 <> ")" ]

-- | [CSS :nth-last-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-last-of-type). This is generated code.
nthLastOfType :: String -> Scope
nthLastOfType x1 = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":nth-last-of-type(" <> x1 <> ")" ]

-- | [CSS :nth-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-of-type). This is generated code.
nthOfType :: String -> Scope
nthOfType x1 = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":nth-of-type(" <> x1 <> ")" ]

-- | [CSS :only-child pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:only-child). This is generated code.
onlyChild :: Scope
onlyChild = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":only-child" ]

-- | [CSS :only-of-type pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:only-of-type). This is generated code.
onlyOfType :: Scope
onlyOfType = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":only-of-type" ]

-- | [CSS :optional pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:optional). This is generated code.
optional :: Scope
optional = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":optional" ]

-- | [CSS :out-of-range pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:out-of-range). This is generated code.
outOfRange :: Scope
outOfRange = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":out-of-range" ]

-- | [CSS :picture-in-picture pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:picture-in-picture). This is generated code.
pictureInPicture :: Scope
pictureInPicture = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":picture-in-picture" ]

-- | [CSS :placeholder-shown pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:placeholder-shown). This is generated code.
placeholderShown :: Scope
placeholderShown = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":placeholder-shown" ]

-- | [CSS :paused pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:paused). This is generated code.
paused :: Scope
paused = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":paused" ]

-- | [CSS :playing pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:playing). This is generated code.
playing :: Scope
playing = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":playing" ]

-- | [CSS :read-only pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:read-only). This is generated code.
readOnly :: Scope
readOnly = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":read-only" ]

-- | [CSS :read-write pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:read-write). This is generated code.
readWrite :: Scope
readWrite = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":read-write" ]

-- | [CSS :required pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:required). This is generated code.
required :: Scope
required = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":required" ]

-- | [CSS :right pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:right). This is generated code.
right :: Scope
right = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":right" ]

-- | [CSS :root pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:root). This is generated code.
root :: Scope
root = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":root" ]

-- | [CSS :scope pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:scope). This is generated code.
scope :: Scope
scope = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":scope" ]

-- | [CSS :target pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:target). This is generated code.
target :: Scope
target = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":target" ]

-- | [CSS :valid pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:valid). This is generated code.
valid :: Scope
valid = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":valid" ]

-- | [CSS :visited pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:visited). This is generated code.
visited :: Scope
visited = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":visited" ]

-- | [CSS :where pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:where). This is generated code.
where_ :: String -> Scope
where_ x1 = Scope $ mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ ":where(" <> x1 <> ")" ]

