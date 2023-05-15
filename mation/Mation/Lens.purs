
-- | Lenses and lens combinators of particular use in mation applications

module Mation.Lens where

import Prelude

import Record as Rec
import Prim.Row (class Union, class Nub, class Cons, class Lacks)
import Prim.RowList (class RowToList, Cons)
import Type.Proxy (Proxy (..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Symbol (class IsSymbol)
import Data.Lens (Lens, Lens', view, lens, set)
import Data.Lens.Record (prop)


-- | Product of disjoint lenses
-- |
-- | The lenses MUST be disjoint or the product will fail the lens laws
product :: forall s a b. Lens' s a -> Lens' s b -> Lens' s (a /\ b)
product la lb = lens
  (\s -> view la s /\ view lb s)
  (\s (a /\ b) -> s # set la a # set lb b)

infixr 4 product as ×

-- | Combine two record lenses
-- |
-- | Example:
-- | ```
-- | len1 :: Lens' Cat { name :: String }
-- | len2 :: Lens' Cat { ageYears :: Number }
-- | productL len1 len2 :: Lens' Cat { name :: String, ageYears :: Number }
-- | ```
productR ::
  forall s (a :: Row Type) (b :: Row Type) ab.
  Union a b ab => Nub ab ab =>
  GetAll a b ab => GetAll b a ab =>
  Lens' s (Record a) -> Lens' s (Record b) -> Lens' s (Record ab)
productR la lb = lens
  (\s -> view la s `Rec.merge` view lb s)
  (\s rec -> let
      a = getAll (Proxy :: Proxy a) (Proxy :: Proxy b) rec
      b = getAll (Proxy :: Proxy b) (Proxy :: Proxy a) rec
    in s # set la a # set lb b)

infixr 4 productR as ××

-- | Supplies `getAll`, which extracts a subrecord from a record
class
  GetAll (x :: Row Type) (y :: Row Type) xy | x xy -> x
where
  getAll :: Proxy x -> Proxy y -> Record xy -> Record x

instance Union () y xy => GetAll () y xy where
  getAll Proxy Proxy _xy = ({} :: Record ())

else instance

      -- Let 'label' be the label of the first row in 'x'
  ( RowToList x (Cons label xlisthead xlisttail)
      -- Deconstruct 'x' into 'xhead' and 'xtail' via 'label'
  , Cons label xhead xtail x
      -- Deconstruct 'xy' into 'xyhead' and 'xytial' via 'label'
  , Cons label xhead xytail xy
      -- Witness to 'label' being a symbol
  , IsSymbol label
      -- Needed for 'delete'
  , Lacks label xytail
      -- Needed for 'insert'
  , Lacks label xtail
      -- Recur
  , GetAll xtail y xytail

) => GetAll x y xy where

  getAll Proxy Proxy (xy :: Record xy) =
    let
      (head :: xhead) = Rec.get (Proxy :: Proxy label) xy
      (rest :: Record xtail) = getAll (Proxy :: Proxy xtail) (Proxy :: Proxy y) (Rec.delete (Proxy :: Proxy label) xy)
    in
    Rec.insert (Proxy :: Proxy label) head rest


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


-- | Construct a lens targeting a field of a record.
-- | Wrap the lens target in a single-field record.
-- |
-- | Example:
-- | ```
-- | type Cat = { name :: String, ageYears :: Number }
-- |
-- | nameLens :: Lens' Cat { name :: String }
-- | nameLens = fieldL (Proxy :: Proxy "name")
-- | ```
fieldR ::
  forall label r a b ra rb justA justB.
  IsSymbol label =>
  Cons label a r ra => Cons label b r rb =>
  Cons label a () justA => Cons label b () justB =>
  Proxy label -> Lens (Record ra) (Record rb) (Record justA) (Record justB)
fieldR l = lens
  (\rec -> Rec.insert l (Rec.get l rec) {})
  (\rec val -> Rec.set l (Rec.get l val) rec)

-- | Construct a lens targeting a subrecord.
-- |
-- | Example:
-- | ```
-- | type Cat = { name :: String, ageYears :: Number, ownerName :: String }
-- |
-- | subrecordLens :: Lens' Cat { name :: String, ageYears :: Number }
-- | subrecordLens = subrecord
-- | ```
subrecord :: forall x y xy.
  Union x y xy => Nub xy xy => GetAll x y xy => GetAll y x xy =>
  Lens' (Record xy) (Record x)
subrecord = lens
  (getAll (Proxy :: Proxy x) (Proxy :: Proxy y))
  (\rec subrec -> subrec `Rec.merge` getAll (Proxy :: Proxy y) (Proxy :: Proxy x) rec)



type Cat = { name :: String, ageYears :: Number, ownerName :: String }

subcat :: Lens' Cat { name :: String, ageYears :: Number }
subcat = subrecord

lName :: Lens' Cat String
lName = field (Proxy :: Proxy "name")

lAge :: Lens' Cat Number
lAge = field (Proxy :: Proxy "ageYears")

lOwner :: Lens' Cat String
lOwner = field (Proxy :: Proxy "ownerName")

lAllTuple :: Lens' Cat (String /\ Number /\ String)
lAllTuple = lName × lAge × lOwner

lNameR :: Lens' Cat { name :: String }
lNameR = fieldR (Proxy :: Proxy "name")

lAgeR :: Lens' Cat { ageYears :: Number }
lAgeR = fieldR (Proxy :: Proxy "ageYears")

lOwnerR :: Lens' Cat { ownerName :: String }
lOwnerR = fieldR (Proxy :: Proxy "ownerName")

lTwoRecord :: Lens' Cat { name :: String, ageYears :: Number }
lTwoRecord = lNameR ×× lAgeR

lTwoRecord' :: Lens' Cat { name :: String, ageYears :: Number }
lTwoRecord' = subrecord

lAllRecord :: Lens' Cat Cat
lAllRecord = lNameR ×× lAgeR ×× lOwnerR


