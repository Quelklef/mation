
-- | CSS selectors
module Mation.Selectors (module X, module Mation.Selectors) where
  
import Mation.Gen.PseudoClasses as X

import Mation.Core.Prelude
import Mation.Core.Style as S
import Mation.Styles (Scope (..))
import Mation.Core.Util.PuncturedFold as PF
import Mation.Core.Util.FreeMonoid as FM


-- | Represents a CSS selector, such as `#my-element > *:first-child`
type Selector = String

-- | `{sel} > {sel}`
childrenWhere :: Selector -> Scope
childrenWhere sel = Scope $ S.mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem " > ", PF.Elem sel ]

-- | `{sel} > *`
children :: Scope
children = childrenWhere "*"

-- | `{sel} {sel}`
descendantsWhere :: Selector -> Scope
descendantsWhere sel = Scope $ S.mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem " ", PF.Elem sel ]

-- | `{sel} *`
descendants :: Scope
descendants = descendantsWhere "*"

-- | `{sel}.{class}`
class_ :: String -> Scope
class_ cls = Scope $ S.mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem $ "." <> cls ]

-- | `{sel}[{attribute}]`
attribute :: String -> Scope
attribute attr = Scope $ S.mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem "[", PF.Elem attr, PF.Elem "]" ]

-- | `{sel} + {sel}`
next :: Selector -> Scope
next sel = Scope $ S.mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem " + ", PF.Elem sel ]

-- | `{sel} ~ {sel}`
following :: Selector -> Scope
following sel = Scope $ S.mkSelectorScope $ PF.PF [ PF.Hole, PF.Elem " ~ ", PF.Elem sel ]

-- | CSS `@media` at-rule
-- |
-- | Any necessary parentheses need to be passed in by you.
-- | That is, you must write `media "(max-width: 500px)"` instead of
-- | writing `media "max-width: 500px"`. This is because not all
-- | valid `@media` conditions have enclosing parentheses.
media :: String -> Scope
media s = Scope $ S.mkBlockScope $ "@media " <> s

-- | CSS `@supports` at-rule
-- |
-- | Any necessary parentheses need to be passed in by you. (see `media`)
supports :: String -> Scope
supports s = Scope $ S.mkBlockScope $ "@supports " <> s

-- | CSS `@document` at-rule
-- |
-- | Any necessary parentheses need to be passed in by you. (see `media`)
document :: String -> Scope
document s = Scope $ S.mkBlockScope $ "@document " <> s


-- | Performs scope composition. On selector-level scopes this works as one might expect: the
-- | composition `children >> firstChild` corresponds to the CSS `> *:first-child`, and the
-- | composition `firstChild >> children` corresponds to `:first-child > *`.
-- |
-- | Composing with a block-level scope adds a static condition to the selector. For instance,
-- | the selector `children >> firstChild >> media "print"` is the same as `children >> firstChild`
-- | if the `@media print` rule holds, and is the empty selector otherwise.
-- |
-- | Note that it does not matter where block scopes come in composition chains; the result
-- | will always be the same. That is, all of the following are equivalent:
-- |
-- | ```
-- | children >> firstChild >> media "print"
-- | children >> media "print" >> firstChild
-- | media "print" >> children >> firstChild
-- | ```
-- |
-- | As it so happens, the generated CSS will always put block-level selectors on the "outside".
-- | The above scopes will all produce the same CSS when applied to the selector `#my-elem` and
-- | styles `color: blue`; namely, this CSS is
-- |
-- | ```
-- | @media print { #my-elem > *:first-child { color: blue; } }
-- | ```
-- |
-- | ***
-- |
-- | It may seem a little weird to allow block-level scopes and selector-level scopes to be
-- | composed. They seem in some sense like "different beasts": a block-level scope is
-- | a *condition* on the state of the *page*, whereas a selector-level scope is a *transformation*
-- | on a collection of *DOM nodes*.
-- |
-- | However, there is a reasonable abstraction under which both block- and selector-level scopes
-- | fall, where the composition given by `>>` is highly natural. I present that now, for your
-- | reading pleasure.
-- |
-- | We give a denotation to CSS selectors (eg `p:hover`), selector scopes (eg `:empty`, `> *`),
-- | and block scopes (eg `@media print`). The domain is
-- |
-- | ```
-- | type Sel = Page -> Set DomNode -> Set DomNode
-- | ```
-- |
-- | Here `Page` is the type of page states. A page state includes information such as viewport
-- | size as well as the current state of the DOM.
-- |
-- | A CSS selector becomes a `Sel` by ignoring the given `Set DomNode` and producing a new
-- | set of nodes from the current DOM. For instance, the selector `*` is thought of as the
-- | function `\page _ -> allNodes page.dom`, and the selector `p:hover` is thought of as
-- | the function `\page _ -> filter isHovered $ allNodes page.dom`.
-- |
-- | A selector scope becomes a `Sel` in the natural way. The scope `:hover` is thought of
-- | as the function `\_ nodes -> filter isHovered nodes`, and the scope `+ *` is thought
-- | of as the function ``\page nodes -> page.dom # filter (\n -> any (n `isAfter` _) nodes)``.
-- |
-- | Finally, a block selector becomes a `Sel` by filtering the given `Set DomNode` based
-- | on its condition and either producing the empty set or producing the given set unchanged.
-- | For instance, `@media print` is thought of as the
-- | function `\page nodes -> if page.isPrinting then nodes else mempty`.
-- |
-- | The domain `Sel` gives a reasonable unifying abstraction for CSS selectors as well as
-- | both kinds of scopes. But here's the kicker. `Sel` admits a natural composition; namely,
-- |
-- | ```
-- | comp :: Sel -> Sel -> Sel
-- | comp f g = \page -> f page >>> g page
-- | ```
-- |
-- | This `comp` exactly corresponds to `>>`! In other words, while it may seem strange
-- | to be able to use `>>` on both block- and selector-level scopes, if we think of those
-- | scopes as elements of `Sel` then `>>` is just `comp`, which perhaps feels more natural.
-- |
-- | (Note that this is also exactly `<>` under `Sel' = Page -> Op (Endo (->) (Set DomNode))`
-- | where `Op` flips `<>`)
infixl 1 composeSelectorsLTR as >>

-- | Reverse form of `<<`
infixl 1 composeSelectorsRTL as <<

-- | Left-to-right scope composition
composeSelectorsLTR :: Scope -> Scope -> Scope
composeSelectorsLTR (Scope a) (Scope b) = Scope (b <> a)

-- | Right-to-left scope composition
composeSelectorsRTL :: Scope -> Scope -> Scope
composeSelectorsRTL a b = composeSelectorsLTR b a
