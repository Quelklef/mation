module Mation.Core.Util.IsEndo where

import Mation.Core.Prelude


-- | An instance `IsEndo endoA a` witnesses that `endoA` models
-- | the type `Endo (->) a`; it does so by providing a homomorphism
-- | into `Endo (->) a` that preserves both composition and
-- | pointwise-concatenation
class IsEndo endoA a | endoA -> a where

  -- | The homomorphism
  runEndo :: endoA -> (a -> a)

  -- | Left-to-right composition
  -- |
  -- | Law: ``runEndo (f `composeEndoLTR` g) = runEndo f >>> runEndo g``
  composeEndoLTR :: endoA -> endoA -> endoA

  -- | Pointwise-concatenation
  -- |
  -- | Law: ``runEndo (f `concatEndo` g) = runEndo f <> runEndo g``
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

