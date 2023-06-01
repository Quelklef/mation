module Mation.Core.Daemon where

import Mation.Core.Prelude
import Mation.Core.Util.WRef (WRef)
import Mation.Core.Util.WRef as WRef


-- | A `Daemon m s` is a possibly-long-lived effectful process
-- | which has read/write access to some state of type `s`
-- |
-- | This is to be contrasted with a `Mation m s` which
-- | has write/update access but not read access
type Daemon m s = WRef s -> m Unit

type Daemon' s = Daemon Effect s

enroot :: forall m large small. Lens' large small -> Daemon m small -> Daemon m large
enroot len f = f <<< WRef.mkView len

-- | Transform the underlying monad of a `Daemon`
-- |
-- | The given `m ~> n` is expected to be a monad morphism
hoist :: forall m n s. (m ~> n) -> Daemon m s -> Daemon n s
hoist nt dae = dae >>> nt

