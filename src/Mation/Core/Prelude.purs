
-- | This is the prelude used internally in the Mation codebase
-- |
-- | This module is not intended to be consumed by clients

module Mation.Core.Prelude (module X, module Mation.Core.Prelude) where
  
import Prelude as X

import Effect (Effect) as X
import Effect.Class (class MonadEffect, liftEffect) as X
import Foreign (Foreign) as X
import Type.Proxy (Proxy (..)) as X
import Prim.Coerce (class Coercible) as X
import Safe.Coerce (coerce) as X
import Data.Generic.Rep (class Generic) as X
import Data.Show.Generic (genericShow) as X
import Data.Foldable (class Foldable, fold, foldMap, intercalate, elem) as X
import Data.Tuple.Nested ((/\), type (/\)) as X
import Data.Either (Either (..)) as X
import Data.Maybe (Maybe (..), fromMaybe) as X
import Data.Map (Map) as X
import Data.Monoid (power) as X
import Data.Monoid.Endo (Endo (..)) as X
import Data.Lens.Types (Getter') as X
import Data.Lens.Lens (Lens') as X
import Data.Lens.Setter (Setter', (.~), (%~)) as X
import Data.Lens.Getter ((^.)) as X
import Data.Lens.Record (prop) as X
import Data.Newtype (class Newtype) as X

type Endo' a = X.Endo (->) a
