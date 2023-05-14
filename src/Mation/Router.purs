
-- | Provides two-way sync between a rich route type and the browser address

module Mation.Router (QueryParams, lookup, lookup', Path, emptyPath, Router, mkRouter, readRoute, writeRoute, sync) where

import Data.Nullable (Nullable, toNullable, toMaybe)

import Mation.Core.Prelude
import Mation.Core.Daemon (Daemon)
import Mation.Core.Util.WRef as WRef
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Assoc as Assoc


-- | Abstract representation of a URL
-- |
-- | Does not preserve all information contained in a URL; for instance,
-- | does not encode whether the URL has a trailing slash.
type Path =
    -- | The `/`-delimited parts of the path
  { parts :: Array String
    -- | Query parameters
  , params :: QueryParams
    -- | Excludes the `#`
  , fragment :: Maybe String
  }

type QueryParams = Assoc String (Maybe String)

emptyPath :: Path
emptyPath =
  { parts: []
  , params: mempty
  , fragment: Nothing
  }

-- | Look up a key in some query params
-- |
-- | Returns:
-- | * `Nothing` if the key is not present
-- | * `Just Nothing` if the key is present but has no value (no `=` in the path)
-- | * `Just val` if the key is present and has a value
lookup :: String -> QueryParams -> Maybe (Maybe String)
lookup = Assoc.lookup

-- | Look up a key in some query params
-- |
-- | Does not distinguish between missing keys and valueless keys
lookup' :: String -> QueryParams -> Maybe String
lookup' k qp = join (Assoc.lookup k qp)


data Router route = Router
  { toPath :: route -> Path
  , fromPath :: Path -> route
  }

-- | Create a router
-- |
-- | Note that `fromPath` is total; the return value must
-- | be `route` rather than eg. `Maybe route`. This means that your
-- | route type must be able to represent bad routes, for instance
-- | by including a 404 page.
mkRouter :: forall route. { toPath :: route -> Path, fromPath :: Path -> route } -> Router route
mkRouter = Router

readRoute :: forall route. Router route -> Effect route
readRoute (Router { fromPath }) = fromPath <$> readPath

writeRoute :: forall route. route -> Router route -> Effect Unit
writeRoute route (Router { toPath }) = writePath (toPath route)

-- | Initialize a two-way sync between your custom-defined `route`
-- | type and the browser URL
sync :: forall route. Router route -> Daemon Effect route
sync router wref = do

  -- Sync route->path
  wref # WRef.onChange (\route -> writeRoute route router)

  -- FIXME: sync forward/back buttons

  -- Push path->route
  readRoute router >>= (\route -> WRef.set route wref)



readPath :: Effect Path
readPath = readPath_f toMaybe

writePath :: Path -> Effect Unit
writePath = writePath_f toNullable

foreign import readPath_f :: ToMaybe -> Effect Path
foreign import writePath_f :: ToNullable -> Path -> Effect Unit

type ToNullable = forall a. Maybe a -> Nullable a
type ToMaybe = forall a. Nullable a -> Maybe a

