module Mation.Core.MationT (module Mation.Core.MationT, module X) where

import Mation.Core.Mation (Step) as X

import Mation.Core.Prelude

import Mation.Core.Mation (Step, enrootStep)


-- | Isomorphism of `ReaderT m (Step s)`
-- |
-- | This generalizes `Mation` to a monad by the
-- | correspondence `Mation m s ~ MationT m s Unit`
-- |
-- | This type is not exposed to the framework user
-- | but is used internally
newtype MationT :: forall k. (k -> Type) -> Type -> k -> Type
newtype MationT m s a = MationT (Step s -> m a)

-- | Destructor
runMationT :: forall m s a. MationT m s a -> (Step s -> m a)
runMationT (MationT m) = m

-- | `Semigroup` ala action sequencing
instance (Applicative m, Monoid a) => Semigroup (MationT m s a) where
  append (MationT m) (MationT n) = MationT (\st -> append <$> m st <*> n st)

-- | `Monoid` ala action sequencing
instance (Applicative m, Monoid a) => Monoid (MationT m s a) where
  mempty = MationT (\_ -> pure mempty)

instance Functor m => Functor (MationT m s) where
  map f (MationT ma) = MationT (map (map f) ma)

-- | Change the underlying monad
hoist :: forall m m' s a. (m ~> m') -> MationT m s a -> MationT m' s a
hoist f (MationT m) = MationT (\st -> f (m st))

-- | Monad transformer `lift`
-- |
-- | `MationT` isn't an instance of `MonadTrans` because the
-- | type variables are in the wrong order: we choose to place
-- | `m` before `s` to match `Mation`, which has `m` before `s`
-- | because it's more useful for type-level partial application
lift :: forall m s a. m a -> MationT m s a
lift ma = MationT (const ma)

-- | Contravariant enroot
enroot :: forall m large small a. Setter' large small -> MationT m small a -> MationT m large a
enroot lens (MationT m) = MationT (enrootStep lens >>> m)

