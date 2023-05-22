module Mation.Props.Unsafe where

import Mation.Core.Prelude
import Mation.Core.MationT (Step, MationT (..))
import Mation.Core.MationT as MationT
import Mation.Core.Prop (Prop)
import Mation.Core.Prop as Prop
import Mation.Core.Dom (DomNode)
import Mation.Core.Util.Revertible as Rev


-- | Create a `Prop` which executes a given function on the rendered DOM node
-- | and then execute the given `restore` before rendering the next frame
-- |
-- | This is considered unsafe because it provides the full power of `Effect`
-- | at *render* time, which is easily misused
fixup :: forall m s. (DomNode -> Effect { restore :: Effect Unit }) -> Prop m s
fixup f = Prop.mkFixup \node -> Rev.mkRevertibleE (_.restore <$> f node)

-- | Like `fixup` but lives in `m`
fixupM :: forall m s. Functor m => (DomNode -> m { restore :: m Unit }) -> Prop m s
fixupM f =
  Prop.mkFixup \node -> Rev.mkRevertibleM $ MationT \_step ->
    (MationT.lift <<< _.restore) <$> f node

-- | Like `fixup` but with access the application `Step s`
-- |
-- | Apologies for the `MonadEffect` constraint (programming is hard)
fixup' :: forall m s. MonadEffect m => (DomNode -> Step s -> Effect { restore :: Effect Unit }) -> Prop m s
fixup' f = fixupM' (\node step -> f node step # liftEffect # map (_restore %~ liftEffect))
  where _restore = prop (Proxy :: Proxy "restore")

-- | Like `fixup` lives in `m` and the application `Step s`
fixupM' :: forall m s. Functor m => (DomNode -> Step s -> m { restore :: m Unit }) -> Prop m s
fixupM' f =
  Prop.mkFixup \node -> Rev.mkRevertibleM $ MationT \step ->
    (MationT.lift <<< _.restore) <$> f node step

