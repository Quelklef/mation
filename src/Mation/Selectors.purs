
-- | CSS selectors
module Mation.Selectors (module X, module Mation.Selectors) where
  
import Mation.Gen.PseudoClasses as X
import Mation.Styles ((#>>), (#<<), (#<>)) as X

import Mation.Core.Prelude
import Mation.Core.Style as S
import Mation.Styles (Scope (..))
import Mation.Core.Util.Weave as W


-- | Represents a CSS selector, such as `#my-element > *:first-child`
type Selector = String

-- | `{sel} > {sel}`
childrenWhere :: Selector -> Scope
childrenWhere sel = ScopeAlts [ { selector: W.Weave [ W.Hole, W.Elem " > ", W.Elem sel ], block: W.noop } ]

-- | `{sel} > *`
children :: Scope
children = childrenWhere "*"

-- | `{sel} {sel}`
descendantsWhere :: Selector -> Scope
descendantsWhere sel = ScopeAlts [ { selector: W.Weave [ W.Hole, W.Elem " ", W.Elem sel ], block: W.noop } ]

-- | `{sel} *`
descendants :: Scope
descendants = descendantsWhere "*"

-- | `{sel}.{class}`
class_ :: String -> Scope
class_ cls = ScopeAlts [ { selector: W.Weave [ W.Hole, W.Elem $ "." <> cls ], block: W.noop } ]

-- | `{sel}[{attribute}]`
attribute :: String -> Scope
attribute attr = ScopeAlts [ { selector: W.Weave [ W.Hole, W.Elem "[", W.Elem attr, W.Elem "]" ], block: W.noop } ]

-- | `{sel} + {sel}`
next :: Selector -> Scope
next sel = ScopeAlts [ { selector: W.Weave [ W.Hole, W.Elem " + ", W.Elem sel ], block: W.noop } ]

-- | `{sel} ~ {sel}`
following :: Selector -> Scope
following sel = ScopeAlts [ { selector: W.Weave [ W.Hole, W.Elem " ~ ", W.Elem sel ], block: W.noop } ]

-- | CSS `@media` at-rule
-- |
-- | Any necessary parentheses need to be passed in by you.
-- | That is, you must write `media "(max-width: 500px)"` instead of
-- | writing `media "max-width: 500px"`. This is because not all
-- | valid `@media` conditions have enclosing parentheses.
media :: String -> Scope
media s = ScopeAlts [ { block: W.Weave [ W.Elem "@media ", W.Elem s, W.Elem " { ", W.Hole, W.Elem " } " ], selector: W.noop } ]

-- | CSS `@supports` at-rule
-- |
-- | Any necessary parentheses need to be passed in by you. (see `media`)
supports :: String -> Scope
supports s = ScopeAlts [ { block: W.Weave [ W.Elem "@supports ", W.Elem s, W.Elem " { ", W.Hole, W.Elem " } " ], selector: W.noop } ]

-- | CSS `@document` at-rule
-- |
-- | Any necessary parentheses need to be passed in by you. (see `media`)
document :: String -> Scope
document s = ScopeAlts [ { block: W.Weave [ W.Elem "@document ", W.Elem s, W.Elem " { ", W.Hole, W.Elem " } " ], selector: W.noop } ]

-- | Selects whatever is currently selected. That is, "does nothing"
-- |
-- | This is sometimes useful to be able to do e.g.
-- |
-- | ```
-- | Sel.on (Sel.this <> Sel.children) [ myStyle ]
-- | ```
-- |
-- | To attach some style to both the targeted node and its children
this :: Scope
this = ScopeAlts [ { block: W.Weave [ W.Hole ], selector: W.noop } ]

