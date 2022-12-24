module Mation.Core.Daemon where

import Mation.Core.Prelude
import Mation.Core.Mation (Mation, runMation)
import Mation.WRef (WRef)
import Mation.WRef as WRef


-- | A `Daemon s` is a possibly-long-lived effectful process
-- | which has read/write access to some state of type `s`
-- |
-- | This is to be contrasted with a `Mation m s` which
-- | has write/update access but not read access
-- |
-- | Note that `Daemon`s have a convenient `Monoid` instance
-- | inherited from `(->)`.
-- | Namely, as long as the underlying monad `m` has a `Monoid`
-- | instance akin to `Effect`, then for daemons `mempty` is the
-- | daemon which does nothing and `d1 <> d2` is the daemon which
-- | first executes `d1` and then executes `d2`.
type Daemon m s = WRef s -> m Unit

enroot :: forall m large small. Lens' large small -> Daemon m small -> Daemon m large
enroot len f = f <<< WRef.mkView len

hoist :: forall m n s. (m ~> n) -> Daemon m s -> Daemon n s
hoist nt dae = dae >>> nt

-- | Execute a mation using a `WRef`
-- |
-- | Useful to run `Mation` values within daemons
execMation :: forall m s. MonadEffect m => WRef s -> Mation m s -> m Unit
execMation ref mation = runMation mation (\f -> liftEffect $ WRef.modify_ f ref)
