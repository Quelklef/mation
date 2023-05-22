
-- | Like Data.Exists
-- | We roll our own to account for any extra needs (arity-2 `Exists`,
-- | or `Exists` with constraints, etc)

module Mation.Core.Util.Exists where

import Mation.Core.Prelude

import Unsafe.Coerce (unsafeCoerce)


foreign import data Exists :: forall k. (k -> Type) -> Type

mkExists :: forall f a. f a -> Exists f
mkExists = unsafeCoerce

runExists :: forall f r. (forall a. f a -> r) -> Exists f -> r
runExists = unsafeCoerce

mapExists :: forall f f' a'. (forall a. f a -> f' a') -> Exists f -> Exists f'
mapExists f = runExists (f >>> mkExists)

