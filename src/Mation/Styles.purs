
-- | Functions dealing with `Style` values

module Mation.Styles (module X, Scope (..), on, (#<<), composeScopesRTL, (#>>), composeScopesLTR, (#<>), disjunct) where
 
import Mation.Gen.Styles as X
import Mation.Core.Style (Style) as X

import Mation.Core.Prelude

import Mation.Core.Style as S
import Mation.Core.Util.Weave as W
import Mation.Core.Util.FreeMonoid as FM


-- | Represents a "style scope", which is a piece of logic dictating
-- | to whom some styling applies. Style scopes are one of:
-- |
-- | - block-level scopes, like `@media (max-width: 500px)` or `@print`
-- | - selector-level scopes, like `:empty` or `> p:second-child`
-- |
-- | Most scope-constructing functions live in `Mation.Selectors`
-- |
-- | Note: selector-level scopes and selectors are not the same
-- | thing! A selector acts to produce a collection of elements
-- | from the DOM. A selector *scope*, on the other hand, modifies
-- | a selector.
--
-- Represented as an array of "alternative" scopes. Holding this
-- array is what allows for `#<>`!
newtype Scope = ScopeAlts (Array (S.Scopes W.Weave))

-- | Apply a style scope to some styles
-- |
-- | For instance,
-- |
-- | ```
-- | import Mation.Styles as S
-- | import Mation.Selectors as Sel
-- | S.on (Sel.children #>> Sel.hover) [ S.color "red" ]
-- | ```
-- |
-- | will apply the `color: red` style to hovered children
on :: Scope -> Array S.Style -> S.Style
on (ScopeAlts alts) styles = S.Style (S.addScope <$> alts <*> FM.float styles)

-- | Compose `Scope`s by "chaining" them.
-- | For instance, on selector-level scopes, the
-- | composition `children #>> firstChild` corresponds to `> *:first-child`, and the
-- | composition `firstChild #>> children` corresponds to `:first-child > *`.
-- |
-- | Composing with a block-level scope adds a static condition to the selector. For instance,
-- | the selector `children #>> firstChild #>> media "print"` is the same as `children #>> firstChild`
-- | when the `@media print` rule holds, and is the empty selector otherwise.
-- |
-- | Also see notes [1] and [2].
-- |
-- | ***
-- |
-- | Note [1]
-- |
-- | Note that it does not matter where block scopes come in composition chains; the result
-- | will always be the same. That is, all of the following are equivalent:
-- |
-- | ```
-- | children #>> firstChild #>> media "print"
-- | children #>> media "print" #>> firstChild
-- | media "print" #>> children #>> firstChild
-- | ```
-- |
-- | ***
-- |
-- | Note [2]
-- |
-- | It may seem a little weird to allow block-level scopes and selector-level scopes to be
-- | composed. They seem in some sense like "different beasts": a block-level scope is
-- | a *condition* on the state of the *page*, whereas a selector-level scope is a *transformation*
-- | on a collection of *DOM nodes*.
-- |
-- | However, there is a reasonable abstraction under which both block- and selector-level scopes
-- | fall, where the composition given by `#>>` is highly natural. I present that now, for your
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
-- | This `comp` exactly corresponds to `#>>`! In other words, while it may seem strange
-- | to be able to use `#>>` on both block- and selector-level scopes, if we think of those
-- | scopes as elements of `Sel` then `#>>` is just `comp`, which perhaps feels more natural.
-- |
-- | (Note that this is also exactly `<>` under `Sel' = Page -> Op (Endo (->) (Set DomNode))`
-- | where `Op` flips `<>`)
infixl 1 composeScopesLTR as #>>

-- | Same as `#>>` but with its arguments reversed
infixl 1 composeScopesRTL as #<<

-- | See `#<<`
composeScopesRTL :: Scope -> Scope -> Scope
composeScopesRTL (ScopeAlts alts) (ScopeAlts alts') = ScopeAlts (compose1 <$> alts <*> alts')
  where
  compose1 :: S.Scopes W.Weave -> S.Scopes W.Weave -> S.Scopes W.Weave
  compose1 s s' = s <> s'  -- Recall that `Weave` is a monoid under composition

-- | See `#>>`
composeScopesLTR :: Scope -> Scope -> Scope
composeScopesLTR = flip composeScopesRTL


-- | Composes `Scope`s ala "also"
-- |
-- | That is, where the selector `children #>> firstChild` targets the node's frist child and the
-- | selector `children #>> lastChild` targets the node's last child, the
-- | selector `(children #>> firstChild) #<> (childen #>> lastChild)` selects both.
-- |
-- | This selector could also be written as `children #>> (firstChild #<> lastChild)`; more
-- | generally we have that `#>>` distributes over `#<>`.
infixl 1 disjunct as #<>

-- | See `#<>`
disjunct :: Scope -> Scope -> Scope
disjunct (ScopeAlts a) (ScopeAlts a') = ScopeAlts (a <> a')
