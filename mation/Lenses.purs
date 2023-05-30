
-- | Lenses and lens combinators of particular use in mation applications

module Mation.Lenses
  ( product
  , (×)
  , product'
  , (⊗)
  , class Split
  , split
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
import Prim.Row (class Union, class Nub, class Cons, class Lacks)
import Prim.RowList (class RowToList, Cons)
import Type.Proxy (Proxy (..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Symbol (class IsSymbol)
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
  Union a b ab => Nub ab ab =>
  Split a b ab =>
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
-- | (a :: { n :: Int }) /\ (b :: { s :: String }) = split r
-- | ```
--
-- FIXME just do this with a foreign import lmao
class
  Split (x :: Row Type) (y :: Row Type) xy
    | x -> x, y -> y  -- (ab)uses <https://github.com/purescript/purescript/issues/4474>
                      -- FIXME: use RowToList instead?
where
  split :: Proxy x -> Proxy y -> Record xy -> Record x /\ Record y

instance Split () y y where
  split Proxy Proxy y = {} /\ y

else instance Split x () x where
  split Proxy Proxy x = x /\ {}

else instance

      -- Let 'xlabel' be the label of the first row in 'x'
  ( RowToList x (Cons xlabel _xlisthead _xlisttail)
      -- Deconstruct 'x' into 'xhead' and 'xtail' via 'xlabel'
  , Cons xlabel xhead xtail x
      -- Witness to 'xlabel' being a symbol
  , IsSymbol xlabel
      -- Needed for 'insert'
  , Lacks xlabel xtail

      -- Do all the same on 'y'-stuff
  , RowToList y (Cons ylabel _ylisthead _ylisttail)
  , Cons ylabel yhead ytail y
  , IsSymbol ylabel
  , Lacks ylabel ytail

      -- Deconstruct 'xy' into a value from 'x', a value from 'y', and a tail
  , Cons xlabel xhead xytail1 xy
  , Cons ylabel yhead xytail xytail1
      -- Needed for 'delete'
  , Lacks xlabel xytail1
  , Lacks ylabel xytail

      -- Recur
  , Split xtail ytail xytail

) => Split x y xy where

  split Proxy Proxy (xy :: Record xy) =
    let

      (xy' :: Record xytail1) = Rec.delete (Proxy :: Proxy xlabel) xy
      (xy'' :: Record xytail) = Rec.delete (Proxy :: Proxy ylabel) xy'

      (xhead :: xhead) = Rec.get (Proxy :: Proxy xlabel) xy
      (yhead :: yhead) = Rec.get (Proxy :: Proxy ylabel) xy'
      (xrest :: Record xtail) /\ (yrest :: Record ytail) =
        split (Proxy :: Proxy xtail) (Proxy :: Proxy ytail) xy''

    in
      (/\)
        (Rec.insert (Proxy :: Proxy xlabel) xhead xrest)
        (Rec.insert (Proxy :: Proxy ylabel) yhead yrest)


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
  Union a b ab => Nub ab ab => Split a b ab =>
  Lens' (Record ab) (Record a)
subrecord = lens
  (split (Proxy :: Proxy a) (Proxy :: Proxy b) >>> fst)
  (\ab a -> let _ /\ b = split (Proxy :: Proxy a) (Proxy :: Proxy b) ab
            in a `Rec.merge` b)

  where
  fst (x /\ _) = x


-- | Like `subrecord` but accepts the target subrecord type as a `Proxy`
subrecord' :: forall a b ab.
  Union a b ab => Nub ab ab => Split a b ab =>
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

