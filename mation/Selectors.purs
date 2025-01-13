
-- | CSS selectors
module Mation.Selectors (module X, module Mation.Selectors) where
  
import Mation.Gen.PseudoClasses as X
import Mation.Core.StyleScopeModifier ((#>>), (#<<), (#<>)) as X

import Mation.Core.Prelude
import Mation.Core.StyleScopeModifier (ScopeModifier (..))
import Mation.Core.Util.Weave as W


-- | Represents a CSS selector, such as `#my-element > *:first-child`
type Selector = String

-- | `{sel} > {sel}`
childrenWhere :: Selector -> ScopeModifier
childrenWhere sel = SMAlts [ { selector: W.that <> W.this (" > " <> sel), block: W.noop } ]

-- | `{sel} > *`
children :: ScopeModifier
children = childrenWhere "*"

-- | `{sel} {sel}`
descendantsWhere :: Selector -> ScopeModifier
descendantsWhere sel = SMAlts [ { selector: W.that <> W.this " " <> W.this sel, block: W.noop } ]

-- | `{sel} *`
descendants :: ScopeModifier
descendants = descendantsWhere "*"

-- | `{sel}.{class}`
class_ :: String -> ScopeModifier
class_ cls = SMAlts [ { selector: W.that <> W.this ("." <> cls), block: W.noop } ]

-- | `{sel}[{attribute}]`
attribute :: String -> ScopeModifier
attribute attr = SMAlts [ { selector: W.that <> W.this ("[" <> attr <> "]"), block: W.noop } ]

-- | `{sel} + {sel}`
next :: Selector -> ScopeModifier
next sel = SMAlts [ { selector: W.that <> W.this (" + " <> sel), block: W.noop } ]

-- | `{sel} ~ {sel}`
following :: Selector -> ScopeModifier
following sel = SMAlts [ { selector: W.that <> W.this (" ~ " <> sel), block: W.noop } ]

-- | `{sel}::before` pseudo-element
before :: ScopeModifier
before = SMAlts [ { selector: W.that <> W.this "::before", block: W.noop } ]

-- | `{sel}::after` pseudo-element
after :: ScopeModifier
after = SMAlts [ { selector: W.that <> W.this "::after", block: W.noop } ]

-- | CSS `@media` at-rule
-- |
-- | Any necessary parentheses need to be passed in by you.
-- | That is, you must write `media "(max-width: 500px)"` instead of
-- | writing `media "max-width: 500px"`. This is because not all
-- | valid `@media` conditions have enclosing parentheses.
media :: String -> ScopeModifier
media s = SMAlts [ { block: W.this ("@media " <> s <> " { ") <> W.that <> W.this " } ", selector: W.noop } ]

-- | CSS `@supports` at-rule
-- |
-- | Any necessary parentheses need to be passed in by you. (see `media`)
supports :: String -> ScopeModifier
supports s = SMAlts [ { block: W.this ("@supports " <> s <> " { ") <> W.that <> W.this " } ", selector: W.noop } ]

-- | CSS `@document` at-rule
-- |
-- | Any necessary parentheses need to be passed in by you. (see `media`)
document :: String -> ScopeModifier
document s = SMAlts [ { block: W.this ("@document " <> s <> " { ") <> W.that <> W.this " } ", selector: W.noop } ]

-- | Selects whatever is currently selected. That is, "does nothing"
-- |
-- | This is sometimes useful to be able to do e.g.
-- |
-- | ```
-- | Sel.on (Sel.this #<> Sel.children) [ myStyle ]
-- | ```
-- |
-- | To attach some style to both the targeted node and its children
this :: ScopeModifier
this = SMAlts [ { block: W.that, selector: W.noop } ]

-- | Create a block modifier directly from a `Weave`
-- |
-- | Example:
-- | ```
-- | whenPrinting = rawBlockModifer (Weave [ Elem "@media print {", Hole, Elem "}" ])
-- | ```
-- |
-- | This should only rarely be necessary
rawBlockModifier :: W.Weave String -> ScopeModifier
rawBlockModifier w = SMAlts [ { block: w, selector: W.noop } ]

-- | Create a selector modifier directly from a `Weave`
-- |
-- | Example:
-- | ```
-- | onChildren = rawSelectorModifier (Weave.that <> Weave.this " > *")
-- | ```
-- |
-- | This should only rarely be necessary
rawSelectorModifier :: W.Weave String -> ScopeModifier
rawSelectorModifier w = SMAlts [ { block: W.noop, selector: w } ]

