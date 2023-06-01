
-- | Like Data.Exists, but we roll our own in cae we need anything
-- | fancy (eg. arity-2 existentials, constrained existentials, ...)

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

