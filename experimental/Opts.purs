module Mation.Experimental.Opts where

import Mation.Core.Prelude


type Opts a = a -> a

-- | Default
def :: forall a. Opts a
def = identity
