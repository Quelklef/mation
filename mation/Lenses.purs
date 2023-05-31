
-- | Lenses and lens combinators of particular use in mation applications

module Mation.Lenses
  ( product
  , (×)
  , product'
  , (⊗)
  , class Keys  -- forced export
  , keys        -- forced export
  , field
  , field'
  , fieldRelabelled
  , subrecord
  , subrecord'
  , labelled
  , relabelled
  ) where

-- FIXME tests for this module would be good

import Prelude

import Record as Rec
import Prim.Row (class Union, class Nub, class Cons)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Type.Proxy (Proxy (..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Lens (Lens, Lens', view, lens, set)
import Data.Lens.Iso (Iso, iso)
import Data.Lens.Record (prop)


-- | Product of disjoint lenses
-- |
-- | The lenses MUST be disjoint or the product will fail the lens laws
-- |
-- | Example:
-- | ```
-- | len1 :: Lens' Cat String
-- | len2 :: Lens' Cat Number
-- | product len1 len2 :: Lens' Cat (String /\ Number)
-- | ```
product :: forall s a b. Lens' s a -> Lens' s b -> Lens' s (a /\ b)
product la lb = lens
  (\s -> view la s /\ view lb s)
  (\s (a /\ b) -> s # set la a # set lb b)

infixr 4 product as %%
infixr 4 product as ×


-- | Combine two record lenses.
-- |
-- | The records must be disjoint (ie, share no fields). If the records share
-- | fields, you will get a compiler error.
-- |
-- | Example:
-- | ```
-- | len1 :: Lens' Cat { name :: String }
-- | len2 :: Lens' Cat { ageYears :: Number }
-- | product' len1 len2 :: Lens' Cat { name :: String, ageYears :: Number }
-- | ```
product' ::
  forall s (a :: Row Type) (b :: Row Type) ab.
  Union a b ab => Nub ab ab => Keys a => Keys b =>
  Lens' s (Record a) -> Lens' s (Record b) -> Lens' s (Record ab)
product' la lb = lens
  (\s -> view la s `Rec.merge` view lb s)
  (\s rec -> let a /\ b = split (Proxy :: Proxy a) (Proxy :: Proxy b) rec
             in s # set la a # set lb b)

infixr 4 product' as @@
infixr 4 product' as ⊗


-- | Supplies `split`, which splits a record into two
-- |
-- | Example:
-- | ```
-- | let r = { n: 1, s: "two" }
-- | (a :: { n :: Int }) /\ (b :: { s :: String }) = split Proxy Proxy r
-- | ```
--
-- Implementation derived from [1]
-- [1]: <https://github.com/rowtype-yoga/purescript-record-studio/blob/6269003c6bc4b241ffde6144958cbc1fa960851a/src/Record/Studio/Shrink.purs>
split :: forall a b ab.
  Union a b ab => Nub ab ab => Keys a => Keys b =>
  Proxy a -> Proxy b -> Record ab -> Record a /\ Record b
split Proxy Proxy record =
  (/\)
    (unsafeTake (keys (Proxy :: Proxy a)) record)
    (unsafeTake (keys (Proxy :: Proxy b)) record)

-- | Extracts a set of keys from a given record and unsafely casts the result
foreign import unsafeTake :: forall a a'. Array String -> Record a -> Record a'

-- | Reify the keys of a row type as a value-level string array
class Keys :: forall k. k -> Constraint
class Keys a where
  keys :: Proxy a -> Array String
instance (RowToList a l, KeysRL l) => Keys a where
  keys Proxy = keysRL (Proxy :: Proxy l)

class KeysRL :: forall k. k -> Constraint
class KeysRL l where
  keysRL :: Proxy l -> Array String
instance (IsSymbol label, KeysRL rest) => KeysRL (RL.Cons label _item rest) where
  keysRL Proxy = Array.cons (reflectSymbol (Proxy :: Proxy label)) (keysRL (Proxy :: Proxy rest))
else instance KeysRL RL.Nil where
  keysRL Proxy = []


-- | Construct a lens targeting a field of a record
-- |
-- | Equivalent to `Data.Lens.Record (prop)`
-- |
-- | Example:
-- | ```
-- | type Cat = { name :: String, ageYears :: Number }
-- |
-- | nameLens :: Lens' Cat String
-- | nameLens = field (Proxy :: Proxy "name")
-- | ```
field ::
  forall label r a b ra rb.
  IsSymbol label => Cons label a r ra => Cons label b r rb =>
  Proxy label -> Lens (Record ra) (Record rb) a b
field = prop


-- | Wraps a type up in a single-field record
-- |
-- | Example:
-- | ```
-- | len :: Iso' Int { n :: Int }
-- | len = labelled (Proxy :: Proxy "n")
-- | ```
labelled :: forall label a b justA justB.
  IsSymbol label => Cons label a () justA => Cons label b () justB =>
  Proxy label -> Iso a b (Record justA) (Record justB)
labelled l = iso (\a -> Rec.insert l a {}) (\rec -> Rec.get l rec)


-- | Changes the label name of a single-field record
-- |
-- | Example:
-- | ```
-- | len :: Iso' { n :: Int } { k :: Int }
-- | len = relabelled (Proxy :: Proxy "k")
-- | ```
-- |
-- | This function takes the target label as a `Proxy` argument but
-- | not the existing label. This optimizes for the common case that
-- | the existing label is determined but the target label is not.
-- | If that is not the case, type annotations can be used to
-- | disambiguate.
relabelled :: forall label label' a b justA justB justA' justB'.
  IsSymbol label => IsSymbol label' =>
  Cons label a () justA => Cons label b () justB =>
  Cons label' a () justA' => Cons label' b () justB' =>
  Proxy label' -> Iso (Record justA) (Record justB) (Record justA') (Record justB')
relabelled l' = iso (\a -> Rec.insert l' (Rec.get l a) {}) (\b -> Rec.insert l (Rec.get l' b) {})
  where l = Proxy :: Proxy label


-- | Construct a lens targeting a field of a record.
-- | Wrap the lens target in a single-field record.
-- |
-- | Example:
-- | ```
-- | type Cat = { name :: String, ageYears :: Number }
-- |
-- | nameLens :: Lens' Cat { name :: String }
-- | nameLens = field' (Proxy :: Proxy "name")
-- | ```
field' ::
  forall label r a b ra rb justA justB.
  IsSymbol label =>
  Cons label a r ra => Cons label b r rb =>
  Cons label a () justA => Cons label b () justB =>
  Proxy label -> Lens (Record ra) (Record rb) (Record justA) (Record justB)
field' l = field l <<< labelled l


-- | Construct a lens targeting a field of a record.
-- | Wrap the lens target in a single-field record with a specified label.
-- |
-- | Example:
-- | ```
-- | type Cat = { name :: String, ageYears :: Number }
-- |
-- | nameLens :: Lens' Cat { catName :: String }
-- | nameLens = fieldRelabelled (Proxy :: Proxy "name") (Proxy :: Proxy "catName")
-- | ```
-- |
-- | The lens `fieldRelabelled l l'` will be somewhat more performant
-- | than `field' l <<< relabelled l'`
fieldRelabelled ::
  forall label label' r a b ra rb justA justB.
  IsSymbol label => IsSymbol label' =>
  Cons label a r ra => Cons label b r rb =>
  Cons label' a () justA => Cons label' b () justB =>
  Proxy label -> Proxy label' -> Lens (Record ra) (Record rb) (Record justA) (Record justB)
fieldRelabelled l l' = field l <<< labelled l'


-- | Construct a lens targeting a subrecord.
-- |
-- | The constructed lens is not type-changing. (I'm not sure a type-changing
-- | lens even theoretically makes sense in this case)
-- |
-- | Example:
-- | ```
-- | type Cat = { name :: String, ageYears :: Number, ownerName :: String }
-- |
-- | subrecordLens :: Lens' Cat { name :: String, ageYears :: Number }
-- | subrecordLens = subrecord
-- | ```
subrecord :: forall a b ab.
  Union a b ab => Nub ab ab => Keys a => Keys b =>
  Lens' (Record ab) (Record a)
subrecord = lens
  (split (Proxy :: Proxy a) (Proxy :: Proxy b) >>> fst)
  (\ab a -> let _ /\ b = split (Proxy :: Proxy a) (Proxy :: Proxy b) ab
            in a `Rec.merge` b)

  where
  fst (x /\ _) = x


-- | Like `subrecord` but accepts the target subrecord type as a `Proxy`
subrecord' :: forall a b ab.
  Union a b ab => Nub ab ab => Keys a => Keys b =>
  Proxy (Record a) -> Lens' (Record ab) (Record a)
subrecord' Proxy = subrecord



-- "tests" --

data A = A
data B = B
data C = C
type ABC = { a :: A, b :: B, c :: C }

t1 :: Lens' ABC (A /\ B)
t1 = field (Proxy :: Proxy "a") %% field (Proxy :: Proxy "b")

t2 :: Lens' ABC (A /\ B /\ C)
t2 = field (Proxy :: Proxy "a") %% field (Proxy :: Proxy "b") %% field (Proxy :: Proxy "c")

t3 :: Lens' ABC { a :: A, c :: C }
t3 = field' (Proxy :: Proxy "a") @@ field' (Proxy :: Proxy "c")

t4 :: Lens' ABC ABC
t4 = field' (Proxy :: Proxy "c") @@ field' (Proxy :: Proxy "b") @@ field' (Proxy :: Proxy "a")

t5 :: Lens' ABC { a :: A, c :: C }
t5 = subrecord

t6 :: Lens' ABC { a :: C, b :: B }
t6 = fieldRelabelled (Proxy :: Proxy "c") (Proxy :: Proxy "a") @@ field' (Proxy :: Proxy "b")

