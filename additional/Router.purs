
-- | Provides two-way sync between a rich route type and the browser address

module Mation.Additional.Router
  ( QueryParams
  , lookup
  , lookup'
  , Path
  , emptyPath
  , Router
  , mkRouter
  , VirtualPath
  , emptyVirtualPath
  , mkVirtualRouter
  , readRoute
  , writeRoute
  , sync
  ) where


import Data.String.Common (split) as Str
import Data.String.Pattern (Pattern (..)) as Str
import Data.Array as Array
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

import Mation (Daemon)
import Mation.Core.Refs as Refs
import Mation.Core.Prelude
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Assoc as Assoc


-- FIXME: this module prolly needs some property tests


-- | Abstract representation of a URL path
-- |
-- | Does not preserve all information contained in a URL path; for
-- | instance, does not encode whether the URL has a trailing slash.
type Path =
    -- | The `/`-delimited parts of the path
  { parts :: Array String
    -- | Query parameters
  , params :: QueryParams
    -- | Excludes the `#`
  , fragment :: Maybe String
  }

-- | URL query parameters
type QueryParams = Assoc String (Maybe String)

emptyPath :: Path
emptyPath =
  { parts: []
  , params: mempty
  , fragment: Nothing
  }

-- | Look up a key in some query parameters
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


-- | Contains a correspondence between some rich route type and a string URL path
data Router route = Router
  { toPathStr :: route -> String
  , fromPathStr :: String -> route
  }

-- | Create a router
-- |
-- | Note that `fromPath` is required to be total; the return type is `route`
-- | rather than, say, `Maybe route`. This means that your route type must be able
-- | to represent bad routes, for instance by including a 404 page.
mkRouter :: forall route. { toPath :: route -> Path, fromPath :: Path -> route } -> Router route
mkRouter { toPath, fromPath } = Router
  { toPathStr: toPath >>> printPath encodeURIComponent
  , fromPathStr: parsePathUnsafe decodeURIComponent >>> fromPath
  }

  where

  -- We're reading the route right from the URL so it should never fail to parse
  parsePathUnsafe :: (String -> String) -> String -> Path
  parsePathUnsafe f s = unsafePartial (fromJust (parsePath f s))


-- | A `VirtualPath` is like a `Path` but is encoded into a URL as a fragment instead
-- | of as a path. Accordingly, a `VirtualPath` contains no `fragment`.
-- |
-- | Virtual paths are useful to host an entire routed "app" from a single endpoint,
-- | which is common for single-page applications. Alternatively, one may not need
-- | virtual paths if they set up their webserver to truncate URL paths.
-- |
-- | Example. The `Path` value
-- |
-- | ```
-- | import Mation.Core.Util.Assoc as A
-- | { parts: ["app", "foods"]
-- | , params: A.fromFoldable [ "veggie" /\ "cabbage" ]
-- | , fragment: "delicious"
-- | }
-- | ```
-- |
-- | corresponds to the URL path
-- |
-- | ```
-- | /app/foods?veggie=cabbage#delicious
-- | ```
-- |
-- | and the `VirtualPath` value
-- |
-- |
-- | ```
-- | import Mation.Core.Util.Assoc as A
-- | { parts: ["app", "foods"]
-- | , params: A.fromFoldable [ "veggie" /\ "cabbage" ]
-- | }
-- | ```
-- |
-- | corresponds to the URL path
-- |
-- | ```
-- | /#/app/foods?veggie=cabbage
-- | ```
type VirtualPath =
  { parts :: Array String
  , params :: QueryParams
  }

emptyVirtualPath :: VirtualPath
emptyVirtualPath =
  { parts: []
  , params: mempty
  }

-- | Create a router which represents paths virtually (ie, using the URL fragment)
-- |
-- | The given `error` is used if the URL fragment fails to parse as a virtual path.
mkVirtualRouter :: forall route.
  { toPath :: route -> VirtualPath
  , fromPath :: VirtualPath -> route
  , error :: String -> route
  } -> Router route
mkVirtualRouter { toPath, fromPath, error } = Router
  { toPathStr: toPath >>> concretize encodeURIComponent >>> printPath dontCode
  , fromPathStr: \pathStr ->
      case do
        actualPath <- parsePath dontCode pathStr
        route <- virtualize decodeURIComponent actualPath
        pure route
      of
        Just virtualPath -> fromPath virtualPath
        Nothing -> error pathStr
  }

  where

  -- Noop encode/decode
  dontCode :: String -> String
  dontCode = identity

  concretize :: (String -> String) -> VirtualPath -> Path
  concretize uriEncode { parts, params } = emptyPath
    { fragment = Just $ printPath uriEncode $ emptyPath { parts = parts, params = params }
    }

  virtualize :: (String -> String) -> Path -> Maybe VirtualPath
  virtualize uriDecode { fragment } = toVirtual <$> parsePath uriDecode (fromMaybe "" fragment)
    where toVirtual { parts, params, fragment: _ } = { parts, params }


printPath :: (String -> String) -> Path -> String
printPath uriEncode { parts, params, fragment } =
  fold
  [ parts
      # intercalate "/"
      # prefix "/"
  , params
      # Assoc.toArray
      # map (\(k /\ mv) -> case mv of
          Just v -> uriEncode k <> "=" <> uriEncode v
          Nothing -> uriEncode k)
      # intercalate "&"
      # prefixIfNonempty "?"
  , case fragment of
      Just f -> "#" <> f
      Nothing -> ""
  ]

  where

  prefixIfNonempty pref s = if s == mempty then s else pref <> s
  prefix pref s = pref <> s


parsePath :: (String -> String) -> String -> Maybe Path
parsePath uriDecode str = do

  (pathNameStr :: String) /\ (queryParamsStr :: String) /\ (mFragment :: Maybe String) <-
    case Str.split (Str.Pattern "?") str of
      [all] -> case Str.split (Str.Pattern "#") all of
        [pathName] -> Just (pathName /\ "" /\ Nothing)
        [pathName, fragment] -> Just (pathName /\ "" /\ Just fragment)
        _ -> Nothing
      [pathName, rest] -> case Str.split (Str.Pattern "#") rest of
        [queryParams] -> Just (pathName /\ queryParams /\ Nothing)
        [queryParams, fragment] -> Just (pathName /\ queryParams /\ Just fragment)
        _ -> Nothing
      _ -> Nothing

  let (parts :: Array String) =
        pathNameStr
        # Str.split (Str.Pattern "/")
        # Array.filter isNonempty
        # map uriDecode

  (params :: QueryParams) <-
    queryParamsStr
    # Str.split (Str.Pattern "&")
    # Array.filter isNonempty
    # traverse (Str.split (Str.Pattern "=") >>> case _ of
        [k] -> Just (uriDecode k /\ Nothing)
        [k, v] -> Just (uriDecode k /\ Just (uriDecode v))
        _ -> Nothing)
    # map Assoc.fromFoldable

  let (fragment :: Maybe String) = mFragment

  pure { parts, params, fragment }

  where

  isNonempty = (_ /= mempty)


foreign import readPathStr :: Effect String
foreign import writePathStr :: String -> Effect Unit
foreign import onPathStrChange :: (String -> Effect Unit) -> Effect Unit

foreign import encodeURIComponent :: String -> String
foreign import decodeURIComponent :: String -> String

foreign import debounce :: forall a. Number -> (a -> Effect Unit) -> Effect (a -> Effect Unit)


-- | Read the current route
readRoute :: forall route. Router route -> Effect route
readRoute (Router { fromPathStr }) = fromPathStr <$> readPathStr

-- | Write the current route
writeRoute :: forall route. route -> Router route -> Effect Unit
writeRoute route (Router { toPathStr }) = writePathStr (toPathStr route)

-- | Initialize a two-way sync between your custom-defined `route`
-- | type and the browser URL
-- |
-- | Each change to the route will append to the browser history
sync :: forall route. Router route -> Daemon Effect route
sync (Router { fromPathStr, toPathStr }) wref = do
  syncdWrite <- wref # Refs.sync (toPathStr >>> writePathStr)
  readPathStr >>= (fromPathStr >>> pure) >>= syncdWrite
  onPathStrChange (fromPathStr >>> syncdWrite)

