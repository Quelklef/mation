module Mation.Mation where

import Mation.Prelude


newtype Mation m s = Mation (Step s -> m Unit)

type Step s = forall n. MonadEffect n => (s -> s) -> n Unit


mkCont :: forall m s. (Step s -> m Unit) -> Mation m s
mkCont = Mation

mkPure :: forall m s. MonadEffect m => (s -> s) -> Mation m s
mkPure endo = Mation \step -> liftEffect (step endo)

mkEff :: forall m s. MonadEffect m => m (s -> s) -> Mation m s
mkEff getEndo = Mation \step -> getEndo >>= \endo -> liftEffect (step endo)

runMation :: forall m s. Mation m s -> ((s -> s) -> Effect Unit) -> m Unit
runMation (Mation f) step = f (step >>> liftEffect)

-- FIXME: I think @Lens'@ can be weakened to @Setter'@
embed :: forall m large small. Lens' large small -> Mation m small -> Mation m large
embed lens (Mation f) =
  Mation \apply -> f \endo -> apply (lens %~ endo)

instance Apply m => Semigroup (Mation m s) where
  append (Mation f) (Mation g) = Mation \step -> f step *> g step

instance Applicative m => Monoid (Mation m s) where
  mempty = Mation \_step -> pure unit

