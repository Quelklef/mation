module Mation.Core.Util.UnsureEq where

import Mation.Core.Prelude

import Data.Array as Array
import Data.Function (on)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Nub) as R
import Record.Unsafe (unsafeGet) as R
import Prim.RowList as RL
import Data.Symbol (reflectSymbol)


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


-- FIXME: have instances try reference equality before anything else


-- | Like `Eq`, but allows a type implement partial equality.
-- |
-- | The canonical example is function types, where if two functions
-- | are identical as references, we know they must be equivalent, but
-- | otherwise we know nothing.
class UnsureEq a where
  unsureEq :: a -> a -> Unsure Boolean

instance UnsureEq Unit where unsureEq _ _ = Surely true
instance UnsureEq Boolean where unsureEq = viaEq
instance UnsureEq Int where unsureEq = viaEq
instance UnsureEq Number where unsureEq = viaEq
instance UnsureEq Char where unsureEq = viaEq
instance UnsureEq String where unsureEq = viaEq
instance UnsureEq Void where unsureEq _ _ = Surely true
instance UnsureEq (Proxy s) where unsureEq _ _ = Surely true

instance (UnsureEq a, UnsureEq b) => UnsureEq (a /\ b) where
  unsureEq (a /\ b) (a' /\ b') = unsureEq a a' `min` unsureEq b b'

instance UnsureEq a => UnsureEq (Array a) where
  unsureEq a b =
    if Array.length a /= Array.length b
    then bottom
    else Array.zipWith unsureEq a b # minimum # fromMaybe top


-- The strategy for implementing `UnsureEq` on record types is essentially copied from [1]
-- [1]: <https://github.com/purescript/purescript-prelude/blob/f4cad0ae8106185c9ab407f43cf9abf05c256af4/src/Data/Show.purs#L62-L91>
instance (R.Nub r r, RL.RowToList r rl, UnsureEqFields rl r) => UnsureEq (Record r) where
  unsureEq = unsureEqFields (Proxy :: Proxy rl)

class UnsureEqFields rl r where
  unsureEqFields :: Proxy rl -> Record r -> Record r -> Unsure Boolean

instance UnsureEqFields RL.Nil r where
  unsureEqFields _ _ _ = top

else instance (IsSymbol lbl, UnsureEq head, UnsureEqFields tail rows) => UnsureEqFields (RL.Cons lbl head tail) rows where
  unsureEqFields _ rec1 rec2 =
    let lbl = reflectSymbol (Proxy :: Proxy lbl)
        head1 = (R.unsafeGet lbl rec1 :: head)
        head2 = (R.unsafeGet lbl rec2 :: head)
    in unsureEq head1 head2 `min` unsureEqFields (Proxy :: Proxy tail) rec1 rec2



-- | Implementation for `unsureEq` via `eq`
viaEq :: forall a. Eq a => (a -> a -> Unsure Boolean)
viaEq a b = Surely (a == b)


-- | Implementation for `unsureEq` via reference equality
viaRef :: forall a. (a -> a -> Unsure Boolean)
viaRef a b = case primEq a b of
  true -> Surely true
  false -> Unsure

foreign import primEq :: forall a b. a -> b -> Boolean
