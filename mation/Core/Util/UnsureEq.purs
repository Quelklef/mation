module Mation.Core.Util.UnsureEq
  ( Unsure (..)
  , perhaps
  , surely
  , class UnsureEq
  , unsureEq
  , class UnsureEqFields
  , unsureEqFields
  , primEq
  , class UnsureEqGeRep
  , gUnsureEq
  , genericUnsureEq
  ) where

import Mation.Core.Prelude

import Data.Array as Array
import Data.Function (on)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Nub) as R
import Record.Unsafe (unsafeGet) as R
import Prim.RowList as RL
import Data.Generic.Rep as G
import Data.HeytingAlgebra (tt, ff)
import Data.Set (Set)
import Data.Set as Set
import Data.Map as Map
import Data.List (List (..))


-- FIXME: I think some of the UnsureEq instances in here
--   are not short-circuiting. Maybe use the HeytingAlgebra instance
--   for Data.Lazy (Lazy) to streamline creating short-circuiting
--   instances? Could possibly make existing short-circuiting instances
--   more readable as well. (or perhaps less readable!)


-- | Intended to be used with `a ~ Boolean` to create
-- | a three-valued logic
data Unsure a = Unsure | Surely a

derive instance Eq a => Eq (Unsure a)
derive instance Functor Unsure

-- | Ala `Maybe`
instance Apply Unsure where
  apply Unsure _ = Unsure
  apply _ Unsure = Unsure
  apply (Surely f) (Surely a) = Surely (f a)

-- | Ala `Maybe`
instance Applicative Unsure where
  pure = Surely

-- | `Surely false < Unsure < Surely true`
instance Ord (Unsure Boolean) where
  compare = compare `on` case _ of
    Surely false -> 0
    Unsure -> 1
    Surely true -> 2

instance Bounded (Unsure Boolean) where
  bottom = Surely false
  top = Surely true

-- | Lifts the boolean `HeytingAlgebra` structure into
-- | an `Unsure` context.
-- |
-- | I'm not sure how better to explain this instance in words,
-- | so instead here's its definition:
-- |
-- | - `ff = bottom`
-- | - `tt = top`
-- | - `conj = min`
-- | - `disj = max`
-- | - `not = map not`
-- | - `implies p q = if p <= q then top else q`
instance HeytingAlgebra (Unsure Boolean) where
  ff = bottom
  tt = top
  conj = min
  disj = max
  not = map not
  implies p q = if p <= q then top else q

-- | Interprets `Unsure` as `true`
perhaps :: Unsure Boolean -> Boolean
perhaps = case _ of
  Surely false -> false
  Unsure -> true
  Surely true -> true

-- | Interprets `Unsure` as `false`
surely :: Unsure Boolean -> Boolean
surely = case _ of
  Surely false -> false
  Unsure -> false
  Surely true -> true


-- | Like `Eq` but allows comparison to produce "I don't know"
-- |
-- | The canonical example is function types, where if two functions
-- | are identical as references, we know they must be equivalent, but
-- | otherwise we know nothing.
-- |
-- | ***
-- |
-- | The mation framework uses UnsureEq to perform caching in some
-- | contexts. When looking up a value, it will compare the lookup
-- | key with the cached key using `unsureEq`. If the result is
-- | anything other than `Surely true`, then the result will
-- | be recomputed.
-- |
-- | The benefit of choosing to use `UnsureEq`
-- | for this instead of `Eq` is that we are able to perform a "best
-- | effort" caching on types which can *sometimes* compute equality,
-- | such as
-- |
-- | ```
-- | data IntOperation = Add Int | Subtract Int | SomethingElse (Int -> Int)
-- | ```
class UnsureEq a where
  unsureEq :: a -> a -> Unsure Boolean

instance UnsureEq Unit where unsureEq _ _ = Surely true
instance UnsureEq Boolean where unsureEq a b = Surely (a `primEq` b)
instance UnsureEq Int where unsureEq a b = Surely (a `primEq` b)
instance UnsureEq Number where unsureEq a b = Surely (a `primEq` b)
instance UnsureEq Char where unsureEq a b = Surely (a `primEq` b)
instance UnsureEq String where unsureEq a b = Surely (a `primEq` b)
instance UnsureEq Void where unsureEq _ _ = Surely true
instance UnsureEq (Proxy s) where unsureEq _ _ = Surely true

instance UnsureEq a => UnsureEq (Maybe a) where
  unsureEq (Just a) (Just b) = unsureEq a b
  unsureEq (Just _) Nothing = Surely false
  unsureEq Nothing (Just _) = Surely false
  unsureEq Nothing Nothing = Surely true

instance (UnsureEq a, UnsureEq b) => UnsureEq (a /\ b) where
  unsureEq = primEqOr \(a /\ b) (x /\ y) ->
    case unsureEq a x of
      Surely false -> Surely false  -- short-circuit
      other -> other && unsureEq b y

instance UnsureEq a => UnsureEq (List a) where
  unsureEq = case _, _ of
    Nil, Nil -> tt
    Cons x xs, Cons y ys -> unsureEq (x /\ xs) (y /\ ys)
    _, _ -> ff

instance UnsureEq a => UnsureEq (Set a) where
  -- Defers to the List implementation
  unsureEq = primEqOr (unsureEq `on` (Set.toUnfoldable :: _ -> List _))

instance UnsureEq v => UnsureEq (Map k v) where
  -- Defers to the List implementation
  unsureEq = primEqOr (unsureEq `on` Map.values)

instance UnsureEq (Effect a) where
  unsureEq = primEqOr ff

instance UnsureEq (a -> b) where
  unsureEq = primEqOr ff

instance UnsureEq a => UnsureEq (Array a) where
  unsureEq = primEqOr \a b ->
    if Array.length a /= Array.length b then ff  -- short-circuit
    else unsureEqArrayImpl a b

-- Short-circuiting implementation for Array
unsureEqArrayImpl :: forall a. UnsureEq a => Array a -> Array a -> Unsure Boolean
unsureEqArrayImpl = unsureEqArrayImpl_f
  { unsureEq
  , and: (&&)
  , isSurelyFalse: (_ == ff)
  , surelyTrue: tt
  }

foreign import unsureEqArrayImpl_f :: forall a.
  { unsureEq :: a -> a -> Unsure Boolean
  , and :: Unsure Boolean -> Unsure Boolean -> Unsure Boolean
  , isSurelyFalse :: Unsure Boolean -> Boolean
  , surelyTrue :: Unsure Boolean
  }
  -> (Array a -> Array a -> Unsure Boolean)


-- The strategy for implementing `UnsureEq` on record types is essentially copied from [1]
-- [1]: <https://github.com/purescript/purescript-prelude/blob/f4cad0ae8106185c9ab407f43cf9abf05c256af4/src/Data/Show.purs#L62-L91>
instance (R.Nub r r, RL.RowToList r rl, UnsureEqFields rl r) => UnsureEq (Record r) where
  unsureEq = unsureEqFields (Proxy :: Proxy rl)

-- | Leaked implementation detail for how `UnsureEq` is implemented on record types
class UnsureEqFields :: forall k. k -> Row Type -> Constraint
class UnsureEqFields rl r where
  unsureEqFields :: Proxy rl -> Record r -> Record r -> Unsure Boolean

instance UnsureEqFields RL.Nil r where
  unsureEqFields _ _ _ = tt

else instance (IsSymbol lbl, UnsureEq head, UnsureEqFields tail rows) => UnsureEqFields (RL.Cons lbl head tail) rows where
  unsureEqFields _ rec1 rec2 =
    if primEq rec1 rec2 then tt else
    let lbl = reflectSymbol (Proxy :: Proxy lbl)
        head1 = (R.unsafeGet lbl rec1 :: head)
        head2 = (R.unsafeGet lbl rec2 :: head)
    in unsureEq head1 head2 && unsureEqFields (Proxy :: Proxy tail) rec1 rec2


-- The strategy for implementing `UnsureEq` on `Generic` types is essentially copied from [2]
-- [2]: <https://github.com/purescript/purescript-prelude/blob/f4cad0ae8106185c9ab407f43cf9abf05c256af4/src/Data/Eq/Generic.purs>
genericUnsureEq :: forall a rep. Generic a rep => UnsureEqGeRep rep => (a -> a -> Unsure Boolean)
genericUnsureEq a a' = gUnsureEq (G.from a) (G.from a')

class UnsureEqGeRep a where
  gUnsureEq :: a -> a -> Unsure Boolean

instance UnsureEqGeRep G.NoConstructors where
  gUnsureEq _ _ = Surely true

instance UnsureEqGeRep G.NoArguments where
  gUnsureEq _ _ = Surely true

instance (UnsureEqGeRep a, UnsureEqGeRep b) => UnsureEqGeRep (G.Sum a b) where
  gUnsureEq (G.Inl a) (G.Inl a') = gUnsureEq a a'
  gUnsureEq (G.Inr b) (G.Inr b') = gUnsureEq b b'
  gUnsureEq _ _ = Surely false

instance (UnsureEqGeRep a, UnsureEqGeRep b) => UnsureEqGeRep (G.Product a b) where
  gUnsureEq (G.Product a b) (G.Product a' b') = gUnsureEq a a' && gUnsureEq b b'

instance UnsureEqGeRep a => UnsureEqGeRep (G.Constructor name a) where
  gUnsureEq = gUnsureEq `on` case _ of G.Constructor a -> a

instance UnsureEq a => UnsureEqGeRep (G.Argument a) where
  gUnsureEq = unsureEq `on` case _ of G.Argument a -> a


-- | Javascript `===`, for use in writing some `UnsureEq` instances
foreign import primEq :: forall a b. a -> b -> Boolean

-- | The call `primEqOr f a b` firsts tests if `a === b`, short-circuiting to
-- | `Surely true` if they are equal, and otherwise defers to `f`.
primEqOr :: forall a b. (a -> b -> Unsure Boolean) -> (a -> b -> Unsure Boolean)
primEqOr f a b =
  case primEq a b of
    true -> tt
    false -> f a b
