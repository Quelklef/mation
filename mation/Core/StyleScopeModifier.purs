module Mation.Core.StyleScopeModifier where

import Mation.Core.Prelude
import Mation.Core.Style (Style, Scopes, addScope, composeScopesLTR)
import Mation.Core.Util.Weave (Weave)
import Mation.Core.Util.FreeMonoid as FM


-- | Represents a "style scope modifier", which is a piece of logic
-- | changing to whom some styling applies. Modifiers are one of:
-- |
-- | - block-level modifiers, like `@media (max-width: 500px)` or `@print`
-- | - selector-level modifiers, like `:empty` or `> p:second-child`
-- |
-- | Find modifiers in `Mation.Selectors`
--
-- Represented as an array of "alternative" scopes. Applying the scope
-- modifier consists of composing a given scope with these alternatives;
-- see `on`.
newtype ScopeModifier = SMAlts (Array (Scopes Weave))


-- | Apply a scope modifier to some styles
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
on :: ScopeModifier -> Array Style -> Style
on (SMAlts alts) styles = FM.wrap (addScope <$> alts <*> FM.float styles)


-- | Compose `ScopeModifiers`s by "chaining" them.
-- |
-- | For instance, on selector-level modifiers, the
-- | composition `children #>> firstChild` corresponds to `> *:first-child`, and the
-- | composition `firstChild #>> children` corresponds to `:first-child > *`.
-- |
-- | Composing with a block-level modifier adds a static condition to the selector. For instance,
-- | the selector `children #>> firstChild #>> media "print"` is the same as `children #>> firstChild`
-- | when the `@media print` rule holds, and is the empty selector otherwise.
-- |
-- | Also see notes [1] and [2].
-- |
-- | ***
-- |
-- | Note [1]
-- |
-- | Note that it does not matter where block-level modifiers come in composition chains; the
-- | result will always be the same. That is, all of the following are equivalent:
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
-- | It may seem a little weird to allow block-level modifiers and selector-level modifiers to be
-- | composed. They seem in some sense like "different beasts": a block-level modifier is
-- | a *condition* on the state of the *page*, whereas a selector-level modifier is a *transformation*
-- | on a collection of *DOM nodes*.
-- |
-- | However, there is a reasonable abstraction under which both block- and selector-level modifiers
-- | fall, where the composition given by `#>>` is highly natural. I present that now, for your
-- | reading pleasure.
-- |
-- | We give a denotation to CSS selectors (eg `p:hover`), selector modifiers (eg `:empty`, `> *`),
-- | and block modifiers (eg `@media print`). The domain is
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
-- | A selector modifier becomes a `Sel` in the natural way. The modifier `:hover` is thought of
-- | as the function `\_ nodes -> filter isHovered nodes`, and the modifier `+ *` is thought
-- | of as the function ``\page nodes -> page.dom # filter (\n -> any (n `isAfter` _) nodes)``.
-- |
-- | Finally, a block selector becomes a `Sel` by filtering the given `Set DomNode` based
-- | on its condition and either producing the empty set or producing the given set unchanged.
-- | For instance, `@media print` is thought of as the
-- | function `\page nodes -> if page.isPrinting then nodes else mempty`.
-- |
-- | The domain `Sel` gives a reasonable unifying abstraction for CSS selectors as well as
-- | both kinds of modifiers. But here's the kicker. `Sel` admits a natural composition; namely,
-- |
-- | ```
-- | comp :: Sel -> Sel -> Sel
-- | comp f g = \page -> f page >>> g page
-- | ```
-- |
-- | This `comp` exactly corresponds to `#>>`! In other words, while it may seem strange
-- | to be able to use `#>>` on both block- and selector-level modifiers, if we think of those
-- | modifiers as elements of `Sel` then `#>>` is just `comp`, which perhaps feels more natural.
-- |
-- | (Note that this is also exactly `<>` under `Sel' = Page -> Op (Endo (->) (Set DomNode))`
-- | where `Op` flips `<>`)
infixl 1 composeScopeModifiersLTR as #>>

-- | Same as `#>>` but with its arguments reversed
infixl 1 composeScopeModifiersRTL as #<<

-- | See `#>>`
composeScopeModifiersLTR :: ScopeModifier -> ScopeModifier -> ScopeModifier
composeScopeModifiersLTR (SMAlts alts) (SMAlts alts') = SMAlts (composeScopesLTR <$> alts <*> alts')

-- | See `#<<`
composeScopeModifiersRTL :: ScopeModifier -> ScopeModifier -> ScopeModifier
composeScopeModifiersRTL = flip composeScopeModifiersLTR


-- | Composes `ScopeModifier`s ala "also"
-- |
-- | That is, where the selector `children #>> firstChild` targets the node's frist child and the
-- | selector `children #>> lastChild` targets the node's last child, the
-- | selector `(children #>> firstChild) #<> (childen #>> lastChild)` selects both.
-- |
-- | This selector could also be written as `children #>> (firstChild #<> lastChild)`; more
-- | generally we have that `#>>` distributes over `#<>`.
infixl 1 disjunct as #<>

-- | See `#<>`
disjunct :: ScopeModifier -> ScopeModifier -> ScopeModifier
disjunct (SMAlts a) (SMAlts a') = SMAlts (a <> a')

