
-- | Pruning is Mation's technique for only re-computing an `Html` value
-- | when it actually changes, which can dramatically improve application
-- | performance.
-- |
-- | To prune a part of your view, prefix its computation with one of
-- | the `prune` variants provided by this module (`prune`, `pruneEq`,
-- | `pruneUeq`, so on). For instance, you might replace
-- |
-- | ```
-- | view :: Stuff -> Html Effect (ReadWrite Stuff)
-- | view stuff =
-- |   expensiveComputation stuff
-- | ```
-- |
-- | with
-- |
-- | ```
-- | view :: Stuff -> Html Effect (ReadWrite Stuff)
-- | view stuff =
-- |   pruneEq "expensive-part" expensiveComputation stuff
-- | ```
-- |
-- | The string `"expensive-part"` is called the *prune key*. Taken
-- | together with the prune keys of all ancestor `Html`, they
-- | constitute the prune's *path*.
-- |
-- | For pruning to work propertly, you must ensure that no two
-- | prune sites have the same path.
-- |
-- | Additionally, for pruning to work properly, the function being
-- | pruned MUST NOT close over any non-constant variables. For
-- | instance, the following is invalid because the pruned function
-- | closes over `stuff1`
-- |
-- | ```
-- | view :: Stuff1 -> Stuff2 -> Html Effect (ReadWrite Stuff)
-- | view stuff1 stuff2 =
-- |   pruneEq "expensive-part" brokenComputation stuff2
-- |   where
-- |   brokenComputation stuff2 =
-- |     E.div [] [ expensiveComputation stuff1
-- |              , expensiveComputation stuff2
-- |              ]
-- | ```
--
-- Details + Technical Notes:
--
-- A pruned node consists of:
--
-- * A "render" function producing a virtual node,
-- * A "parameter" value of type `p`
-- * A "comparison" function p -> p -> Unsure Boolean
-- * A "prune key", which is a string
--
-- As discussed, the mation runtime holds onto prune parameters between
-- renders, recomputing the virtual node with the given render function
-- when the parameter changes (according to the comparison function).
--
-- For this to work properly, the render function must not close over
-- any variables. If it does, then changes to those variables will
-- fail to induce a re-render.
--
-- The prune key is used to pair up pruned nodes in adjacent render
-- frames. For each prune node we compute its "key path", which is the
-- sequence of prune keys in the VDOM path from the VDOM root down to
-- the pruned node. When rendering a pruned node, we check to see if the
-- last frame had any node with the same key path. If it did, we match them
-- up for pruning and perform parameter comparison, re-using the DOM
-- Node from the previous render if the parameters have not changed.
--
-- For this to work, one must ensure that, across adjacent renders,
-- if two prune nodes have the same key path then they also have the
-- same type `p` and render function `p -> Html m s`.
--
-- ***
--
-- One might wonder why we go through all this business with prune keys
-- and key paths instead of just matching up pruned nodes based on position
-- in the VDOM (which is how
-- Elm's [Html.Lazy](https://package.elm-lang.org/packages/elm/html/1.0.0/Html-Lazy)
-- [seems to do it](https://guide.elm-lang.org/optimization/lazy.html)).
--
-- One trouble is that position-based matching is intolerant to position changes.
-- If you were to move a position-pruned node around the VDOM, or add or remove
-- a wrapper node around it, or add or remove a wrapper node around *any ancestor*,
-- the node would unconditionally recompute, even if you haven't changed it.
--
-- Another trouble is that position-based matching has to compare `render`
-- functions, and function comparison has to be done with identity comparison.
-- To avoid render functions comparing as inequal every frame we'd have to either
-- lift the function definition to module-scope or try to guarantee that our
-- purescript compiler performs scope lifitng of closed expressions. Both of these
-- options are not particularly enticing.
--
-- Finally I'd like to note about the use of *key paths* over just *keys*,
-- which in principle would also work. The issue is that whatever we use to
-- match up prune nodes needs to be *globally* unique. If we use plain keys, this
-- would mean that every call to `prune` has to choose a *globally*-unique string.
-- For library code this is plain impossible: who knows what strings the library
-- client will use? (The library code could generate long random strings, I
-- suppose, and have a near-guarantee of uniqueness, but that would be
-- extremely ugly.)
--
-- By using key *paths* to match pruned nodes, we only have to ensure some
-- local structure in order to guarantee global uniqueness of key paths.
--
-- For example, on way to guarantee unique key paths is to ensure that:
--
-- - If a node is given a key, so are its siblings, and all these keys
--   are distinct
-- - All parts of the VDOM with unknown keying are given prune keys
--
-- This is not the only way to ensure globally-unique prune key paths. (In fact,
-- this method is rather stringent -- but it should work!)

module Mation.Elems.Prune where

import Mation.Core.Prelude

import Mation.Core.Html as Html
import Mation.Core.Html (Html)
import Mation.Core.Util.UnsureEq (class UnsureEq, Unsure (..), unsureEq)
import Mation.Core.Util.FreeMonoid as FM

import Prim.Row (class Nub) as R
import Record.Unsafe (unsafeGet) as R
import Prim.RowList as RL
import Data.Symbol as Sym


-- | Prunes a function whose inputs are taken as records (to emulate named arguments)
-- |
-- | The function's input type may be nested (ie, a record of records), and the function may
-- | have multiple such "named argument" inputs (ie, have arity > 1).
-- |
-- | Changes to the function's input values are detected using `===` on the values contained
-- | in the argument records.
-- |
-- | ```
-- | viewCoordinate { x, y } { options: { braceStyle } } =
-- |   case braceStyle of Parens ->  E.text $ "(" <> show x <> ", " <> show y <> ")"
-- |                      Angles ->  E.text $ "<" <> show x <> ", " <> show y <> ">"
-- |
-- | pruneN viewCoordinate { x: someX, y: someY } { options: { braceStyle: someBraceStyle } }
-- | ```
class PruneN p a where
  pruneN :: String -> (p -> a) -> (p -> a)

instance CompareNargs (Record r) => PruneN (Record r) (Html m s) where
  pruneN = Html.mkPrune compareNargs

else instance PruneN p (Html m s) where
  pruneN = Html.mkPrune primCompare

else instance PruneN (p0 /\ p1) a => PruneN p0 (p1 -> a) where
  pruneN key fn p0 p1 = pruneN key (\(p0 /\ p1) -> fn p0 p1) (p0 /\ p1)


class CompareNargs a where
  compareNargs :: a -> a -> Unsure Boolean

instance (R.Nub r r, RL.RowToList r rl, CompareNargsFields rl r) => CompareNargs (Record r) where
  compareNargs = compareNargsFields (Proxy :: Proxy rl)

else instance CompareNargs a where
  compareNargs _ _ = Unsure


class CompareNargsFields :: forall k. k -> Row Type -> Constraint
class CompareNargsFields rl r where
  compareNargsFields :: Proxy rl -> Record r -> Record r -> Unsure Boolean

instance CompareNargsFields RL.Nil r where
  compareNargsFields _ _ _ = Surely true

else instance (Sym.IsSymbol lbl, CompareNargs head, CompareNargsFields tail rows) => CompareNargsFields (RL.Cons lbl head tail) rows where
  compareNargsFields _ rec1 rec2 =
    let lbl = Sym.reflectSymbol (Proxy :: Proxy lbl)
        head1 = (R.unsafeGet lbl rec1 :: head)
        head2 = (R.unsafeGet lbl rec2 :: head)
    in compareNargs head1 head2 && compareNargsFields (Proxy :: Proxy tail) rec1 rec2


-- | Prunes a function with one or more inputs, Changes to the input values
-- | are detected using the inputs' `Eq` instance.
-- |
-- | ```
-- | viewCoordinate x y = E.text $ "(" <> show x <> ", " <> show y <> ")"
-- | pruneEq viewCoordinate someX someY
-- | ```
class PruneEq p a where
  pruneEq :: String -> (p -> a) -> (p -> a)

instance Eq p => PruneEq p (Html m s) where
  pruneEq = Html.mkPrune (\a b -> Surely (a == b))

instance PruneEq (p0 /\ p1) a => PruneEq p0 (p1 -> a) where
  pruneEq key fn p0 p1 = pruneEq key (\(p0 /\ p1) -> fn p0 p1) (p0 /\ p1)


-- | Like `pruneEq`, but changes to input values are detected using
-- | the inputs' `UnsureEq` instance
class PruneTeq p a where
  pruneTeq :: String -> (p -> a) -> (p -> a)

instance PruneTeq p (Html m s) where
  pruneTeq = Html.mkPrune primCompare

instance PruneTeq (p0 /\ p1) a => PruneTeq p0 (p1 -> a) where
  pruneTeq key fn p0 p1 = pruneTeq key (\(p0 /\ p1) -> fn p0 p1) (p0 /\ p1)


-- | Like `pruneeq`, but changes to input values are detected using
-- | the inputs' `UnsureEq` instance
class PruneUeq p a where
  pruneUeq :: String -> (p -> a) -> (p -> a)

instance UnsureEq p => PruneUeq p (Html m s) where
  pruneUeq = Html.mkPrune unsureEq

instance PruneUeq (p0 /\ p1) a => PruneUeq p0 (p1 -> a) where
  pruneUeq key fn p0 p1 = pruneUeq key (\(p0 /\ p1) -> fn p0 p1) (p0 /\ p1)


primCompare :: forall a b. a -> b -> Unsure Boolean
primCompare a b =
  case primEq a b of
    true -> Surely true
    false -> Unsure

-- | Javascript `===`, for use in writing some `UnsureEq` instances
foreign import primEq :: forall a b. a -> b -> Boolean


-- | Removes pruning from an `Html` value *and all descendants*
-- |
-- | This may be necessary in order to ensure validity of the prune tree when
-- | embedding some `Html` from an unknown source
unPrune :: forall m s. Html m s -> Html m s
unPrune = Html.unPrune

-- | Attaches a pruning key to a node but does not actually perform pruning
addKey :: forall m s. String -> Html m s -> Html m s
addKey = FM.map <<< Html.addPruneKey

