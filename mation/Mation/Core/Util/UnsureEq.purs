module Mation.Core.Util.UnsureEq
  ( Unsure (..)
  , perhaps
  , surely
  , class UnsureEq
  , unsureEq
  , class UnsureEqFields
  , unsureEqFields
  , viaEq
  , viaPrim
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


data Unsure a = Unsure | Surely a

derive instance Eq a => Eq (Unsure a)

instance Ord (Unsure Boolean) where
  compare = compare `on` case _ of
    Surely false -> 0
    Unsure -> 1
    Surely true -> 2

instance Bounded (Unsure Boolean) where
  bottom = Surely false
  top = Surely true

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



-- | Like `Eq`, but allows a type implement partial equality.
-- |
-- | The canonical example is function types, where if two functions
-- | are identical as references, we know they must be equivalent, but
-- | otherwise we know nothing.
-- |
-- | ***
-- |
-- | The Mation framework uses UnsureEq to perform certain kinds of
-- | caching. It can compare the new and old cache keys using `unsureEq`
-- | and will recompute the cache value if the result is anything
-- | other than `Surely true`. The benefit of choosing to use `UnsureEq`
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
instance UnsureEq Boolean where unsureEq = viaPrim
instance UnsureEq Int where unsureEq = viaPrim
instance UnsureEq Number where unsureEq = viaPrim
instance UnsureEq Char where unsureEq = viaPrim
instance UnsureEq String where unsureEq = viaPrim
instance UnsureEq Void where unsureEq _ _ = Surely true
instance UnsureEq (Proxy s) where unsureEq _ _ = Surely true

instance (UnsureEq a, UnsureEq b) => UnsureEq (a /\ b) where
  unsureEq ab xy =
    if primEq ab xy then top
    else let a /\ b = ab
             x /\ y = xy
         in case unsureEq a x of
              Surely false -> Surely false
              other -> other `min` unsureEq b y

instance UnsureEq a => UnsureEq (Maybe a) where
  unsureEq (Just a) (Just b) = unsureEq a b
  unsureEq (Just _) Nothing = Surely false
  unsureEq Nothing (Just _) = Surely false
  unsureEq Nothing Nothing = Surely true

instance UnsureEq a => UnsureEq (Array a) where
  unsureEq a b =
    case primEq a b, Array.length a == Array.length b of
      true, _ -> top
      _, false -> bottom
      _, _ -> unsureEqArrayImpl a b


-- Short-circuiting implementation for Array
unsureEqArrayImpl :: forall a. UnsureEq a => Array a -> Array a -> Unsure Boolean
unsureEqArrayImpl = unsureEqArrayImpl_f
  { unsureEq
  , and: min
  , isSurelyFalse: (_ == bottom)
  , surelyTrue: top
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
class UnsureEqFields rl r where
  unsureEqFields :: Proxy rl -> Record r -> Record r -> Unsure Boolean

instance UnsureEqFields RL.Nil r where
  unsureEqFields _ _ _ = top

else instance (IsSymbol lbl, UnsureEq head, UnsureEqFields tail rows) => UnsureEqFields (RL.Cons lbl head tail) rows where
  unsureEqFields _ rec1 rec2 =
    if primEq rec1 rec2 then top else
    let lbl = reflectSymbol (Proxy :: Proxy lbl)
        head1 = (R.unsafeGet lbl rec1 :: head)
        head2 = (R.unsafeGet lbl rec2 :: head)
    in unsureEq head1 head2 `min` unsureEqFields (Proxy :: Proxy tail) rec1 rec2


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
  gUnsureEq (G.Product a b) (G.Product a' b') = gUnsureEq a a' `min` gUnsureEq b b'

instance UnsureEqGeRep a => UnsureEqGeRep (G.Constructor name a) where
  gUnsureEq = gUnsureEq `on` case _ of G.Constructor a -> a

instance UnsureEqGeRep a => UnsureEqGeRep (G.Argument a) where
  gUnsureEq = gUnsureEq `on` case _ of G.Argument a -> a



-- | Implementation for `unsureEq` via `eq`
viaEq :: forall a. Eq a => (a -> a -> Unsure Boolean)
viaEq a b = Surely (a == b)


-- | Implementation for `unsureEq` via primitive triple-equals equality
viaPrim :: forall a. (a -> a -> Unsure Boolean)
viaPrim a b = case primEq a b of
  true -> Surely true
  false -> Unsure

foreign import primEq :: forall a b. a -> b -> Boolean
