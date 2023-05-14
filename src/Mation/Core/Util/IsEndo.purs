module Mation.Core.Util.IsEndo where

import Mation.Core.Prelude


-- | Instances of this class witness that a type `endoA` models
-- | the type `Endo (->) a`
-- | for some `a` by giving a homomorphism into `Endo (->) a` preserving
-- | both composition and pointwise-concatenation
class IsEndo endoA a | endoA -> a where

  -- | The homomorphism
  runEndo :: endoA -> (a -> a)

  -- | Left-to-right composition
  -- |
  -- | Law: ``interp (f `composeLTR` g) = interp f >>> interp g``
  composeEndoLTR :: endoA -> endoA -> endoA

  -- | Pointwise-concatenation
  -- |
  -- | Law: ``interp (f `concat` g) = interp f <> interp g``
  concatEndo :: endoA -> endoA -> endoA

composeEndoRTL :: forall endoA a. IsEndo endoA a => endoA -> endoA -> endoA
composeEndoRTL = flip composeEndoLTR

infixl 5 concatEndo as .<>  -- "point"-wise <>
infixl 4 composeEndoLTR as .>>
infixl 4 composeEndoRTL as .<<

instance Semigroup a => IsEndo (Endo (->) a) a where
  runEndo (Endo f) = f
  composeEndoLTR = flip (<>)
  concatEndo (Endo f) (Endo g) = Endo (f <> g)

toEndo :: forall endo a. IsEndo endo a => endo -> Endo (->) a
toEndo endo = Endo (runEndo endo)
