
-- | Lenses and lens combinators of particular use in mation applications

module Mation.Lenses
  ( product
  , (×)
  , product'
  , (⊗)
  , atprod
  , atprod'
  , field
  , field'
  , subrecord
  , subrecord'
  , labelled
  , relabelled

  -- The classes must be exported for some lens combinators to work
  , class Keys
  , keys
  , class KeysRL
  , keysRL
  ) where

-- FIXME tests for this module would be good

import Prelude  -- Use Prelude instead of Mation.Core.Prelude to make this module easy
                -- to migrate into its own package if ever appropriate

import Record as Rec
import Prim.Row (class Union, class Nub, class Cons)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Type.Proxy (Proxy (..))
import Data.Maybe (fromMaybe)
import Data.Either (Either (..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Lens (Lens, Lens', view, lens, set, preview)
import Data.Lens.Iso (Iso, iso)
import Data.Lens.Record (prop)
import Data.Lens.AffineTraversal (AffineTraversal', affineTraversal)


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


-- | Product of lenses over singleton records
-- |
-- | The records must be disjoint (ie, share no fields), or you will
-- | get a compiler error. (`Nub` will fail to be satisfied)
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
  (\s rec -> let a /\ b = split @a @b rec
             in s # set la a # set lb b)

infixr 4 product' as @@
infixr 4 product' as ⊗


-- | Product of affine traversals
-- |
-- | The supplied traverals MUST be disjoint, or the resultant optic
-- | will violate the following weakened set-get law:
-- |
-- | ```
-- | preview l (set l a s) = Just a
-- | ```
atprod :: forall s a b.
     AffineTraversal' s a
  -> AffineTraversal' s b
  -> AffineTraversal' s (a /\ b)
atprod la lb =
  affineTraversal
    (\s (a /\ b) -> s # set la a # set lb b)
    (\s -> fromMaybe (Left s) do
             a <- preview la s
             b <- preview lb s
             pure $ Right (a /\ b))


-- | Product of affine traverals over singleton records
-- |
-- | This is to `atprod` as `product'` is to `product`
atprod' ::
  forall s (a :: Row Type) (b :: Row Type) ab.
  Union a b ab => Nub ab ab => Keys a => Keys b =>
  AffineTraversal' s (Record a) -> AffineTraversal' s (Record b) -> AffineTraversal' s (Record ab)
atprod' la lb =
  affineTraversal
    (\s rec -> let a /\ b = split @a @b rec
               in s # set la a # set lb b)
    (\s -> fromMaybe (Left s) do
             a <- preview la s
             b <- preview lb s
             pure $ Right (Rec.merge a b))


-- | Construct a lens targeting a field of a record
-- |
-- | Equivalent to `Data.Lens.Record (prop)`
-- |
-- | Example:
-- | ```
-- | type Cat = { name :: String, ageYears :: Number }
-- |
-- | nameLens :: Lens' Cat String
-- | nameLens = field @"name"
-- | ```
field ::
  forall @label r a b ra rb.
  IsSymbol label => Cons label a r ra => Cons label b r rb =>
  Lens (Record ra) (Record rb) a b
field = prop (Proxy :: Proxy label)


-- | Wraps a type up in a single-field record
-- |
-- | Example:
-- | ```
-- | len :: Iso' Int { n :: Int }
-- | len = labelled @"n"
-- | ```
labelled :: forall @label a b justA justB.
  IsSymbol label => Cons label a () justA => Cons label b () justB =>
  Iso a b (Record justA) (Record justB)
labelled = iso (\a -> Rec.insert l a {}) (\rec -> Rec.get l rec)
  where l = (Proxy :: Proxy label)


-- | Changes the label name of a single-field record
-- |
-- | Example:
-- | ```
-- | len :: Iso' { n :: Int } { k :: Int }
-- | len = relabelled @"k"
-- | ```
-- |
-- | This function takes the target label as a `Proxy` argument but
-- | not the existing label. This optimizes for the common case that
-- | the existing label is determined but the target label is not.
-- | If that is not the case, type annotations can be used to
-- | disambiguate.
relabelled :: forall label @label' a b justA justB justA' justB'.
  IsSymbol label => IsSymbol label' =>
  Cons label a () justA => Cons label b () justB =>
  Cons label' a () justA' => Cons label' b () justB' =>
  Iso (Record justA) (Record justB) (Record justA') (Record justB')
relabelled = iso (\a -> Rec.insert l' (Rec.get l a) {}) (\b -> Rec.insert l (Rec.get l' b) {})
  where
  l = Proxy :: Proxy label
  l' = Proxy :: Proxy label'


-- | Construct a lens targeting a field of a record.
-- | Wrap the lens target in a single-field record.
-- |
-- | Example:
-- | ```
-- | type Cat = { name :: String, ageYears :: Number }
-- |
-- | nameLens :: Lens' Cat { name :: String }
-- | nameLens = field' @"name"
-- | ```
field' ::
  forall @label r a b ra rb justA justB.
  IsSymbol label =>
  Cons label a r ra => Cons label b r rb =>
  Cons label a () justA => Cons label b () justB =>
  Lens (Record ra) (Record rb) (Record justA) (Record justB)
field' = field @label <<< labelled @label


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
  (split @a @b >>> fst)
  (\ab a -> let _ /\ b = split @a @b ab
            in a `Rec.merge` b)

  where
  fst (x /\ _) = x


-- | Like `subrecord` but accepts the target subrecord type as a `Proxy`
subrecord' :: forall a b ab.
  Union a b ab => Nub ab ab => Keys a => Keys b =>
  Proxy (Record a) -> Lens' (Record ab) (Record a)
subrecord' Proxy = subrecord



-- | `split` splits a record into two
-- |
-- | Example:
-- | ```
-- | let r = { n: 1, s: "two" }
-- | (a :: { n :: Int }) /\ (b :: { s :: String }) = split Proxy Proxy r
-- | ```
--
-- Implementation derived from [1]
-- [1]: <https://github.com/rowtype-yoga/purescript-record-studio/blob/6269003c6bc4b241ffde6144958cbc1fa960851a/src/Record/Studio/Shrink.purs>
split :: forall @a @b ab.
  Union a b ab => Nub ab ab => Keys a => Keys b =>
  Record ab -> Record a /\ Record b
split record =
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



-- "tests" --

data A = A
data B = B
data C = C
type ABC = { a :: A, b :: B, c :: C }

t1 :: Lens' ABC (A /\ B)
t1 = field @"a" %% field @"b"

t2 :: Lens' ABC (A /\ B /\ C)
t2 = field @"a" %% field @"b" %% field @"c"

t3 :: Lens' ABC { a :: A, c :: C }
t3 = field' @"a" @@ field' @"c"

t4 :: Lens' ABC ABC
t4 = field' @"c" @@ field' @"b" @@ field' @"a"

t5 :: Lens' ABC { a :: A, c :: C }
t5 = subrecord

t6 :: Lens' ABC { x :: A }
t6 = field @"a" <<< labelled @"x"

t7 :: Lens' ABC { x :: A, y :: B }
t7 = (field @"a" <<< labelled @"x") @@ (field @"b" <<< labelled @"y")

