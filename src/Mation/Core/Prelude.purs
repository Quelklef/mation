module Mation.Core.Prelude (module X) where

import Prelude as X

import Effect (Effect) as X
import Effect.Class (class MonadEffect, liftEffect) as X
import Foreign (Foreign) as X
import Type.Proxy (Proxy (..)) as X
import Prim.Coerce (class Coercible) as X
import Safe.Coerce (coerce) as X
import Data.Foldable (class Foldable, foldMap, intercalate) as X
import Data.Unit (Unit, unit) as X
import Data.Tuple.Nested ((/\), type (/\)) as X
import Data.Either (Either (..)) as X
import Data.Maybe (Maybe (..), fromMaybe) as X
import Data.Map (Map) as X
import Data.Lens.Lens (Lens') as X
import Data.Lens.Setter (Setter') as X
import Data.Lens.Record (prop) as X
import Data.Lens.Setter ((.~), (%~)) as X
import Data.Newtype (class Newtype) as X
