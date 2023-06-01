
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
childrenWhere sel = SMAlts [ { selector: W.Weave [ W.Hole, W.Elem " > ", W.Elem sel ], block: W.noop } ]

-- | `{sel} > *`
children :: ScopeModifier
children = childrenWhere "*"

-- | `{sel} {sel}`
descendantsWhere :: Selector -> ScopeModifier
descendantsWhere sel = SMAlts [ { selector: W.Weave [ W.Hole, W.Elem " ", W.Elem sel ], block: W.noop } ]

-- | `{sel} *`
descendants :: ScopeModifier
descendants = descendantsWhere "*"

-- | `{sel}.{class}`
class_ :: String -> ScopeModifier
class_ cls = SMAlts [ { selector: W.Weave [ W.Hole, W.Elem $ "." <> cls ], block: W.noop } ]

-- | `{sel}[{attribute}]`
attribute :: String -> ScopeModifier
attribute attr = SMAlts [ { selector: W.Weave [ W.Hole, W.Elem "[", W.Elem attr, W.Elem "]" ], block: W.noop } ]

-- | `{sel} + {sel}`
next :: Selector -> ScopeModifier
next sel = SMAlts [ { selector: W.Weave [ W.Hole, W.Elem " + ", W.Elem sel ], block: W.noop } ]

-- | `{sel} ~ {sel}`
following :: Selector -> ScopeModifier
following sel = SMAlts [ { selector: W.Weave [ W.Hole, W.Elem " ~ ", W.Elem sel ], block: W.noop } ]

-- | CSS `@media` at-rule
-- |
-- | Any necessary parentheses need to be passed in by you.
-- | That is, you must write `media "(max-width: 500px)"` instead of
-- | writing `media "max-width: 500px"`. This is because not all
-- | valid `@media` conditions have enclosing parentheses.
media :: String -> ScopeModifier
media s = SMAlts [ { block: W.Weave [ W.Elem "@media ", W.Elem s, W.Elem " { ", W.Hole, W.Elem " } " ], selector: W.noop } ]

-- | CSS `@supports` at-rule
-- |
-- | Any necessary parentheses need to be passed in by you. (see `media`)
supports :: String -> ScopeModifier
supports s = SMAlts [ { block: W.Weave [ W.Elem "@supports ", W.Elem s, W.Elem " { ", W.Hole, W.Elem " } " ], selector: W.noop } ]

-- | CSS `@document` at-rule
-- |
-- | Any necessary parentheses need to be passed in by you. (see `media`)
document :: String -> ScopeModifier
document s = SMAlts [ { block: W.Weave [ W.Elem "@document ", W.Elem s, W.Elem " { ", W.Hole, W.Elem " } " ], selector: W.noop } ]

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
this = SMAlts [ { block: W.Weave [ W.Hole ], selector: W.noop } ]

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
-- | onChildren = rawSelectorModifier (Weave [ Hole, Elem " > *" ])
-- | ```
-- |
-- | This should only rarely be necessary
rawSelectorModifier :: W.Weave String -> ScopeModifier
rawSelectorModifier w = SMAlts [ { block: W.noop, selector: w } ]

