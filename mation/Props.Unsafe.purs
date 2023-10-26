module Mation.Props.Unsafe where

import Mation.Core.Prelude
import Mation.Core.MationT (Step, MationT (..))
import Mation.Core.MationT as MationT
import Mation.Core.Prop (Prop)
import Mation.Core.Prop as Prop
import Mation.Core.Dom (DomNode)
import Mation.Core.Util.Revertible as Rev
import Mation.Lenses (field)


-- | Create a `Prop` which executes some arbitrary action (such as adding
-- | a class) on the DOM node during rendering. The action, call it `fix`,
-- | must return a `restore` action which exactly undoes `fix`.
-- |
-- | Fixups can be used to create "custom properties" for your `Html` values.
-- | In fact, the mation `Style` API uses `fixup` under the hood!
-- |
-- | The `fixup` prop is guaranteed to be applied to the target DOM node when the
-- | prop is first added, or when the DOM node first mounts, and is guaranteed
-- | to be restored when the prop is removed, or when the DOM node is about to
-- | be removed from the DOM. Between these points in time, however, no strong
-- | guarantees are made about timing or multiplicity of fixup application
-- | or restoration.
-- |
-- | Because `fixup` allows the user to perform effects at *render* time, it is
-- | considered an unsafe operation.
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
  where _restore = field @"restore"

-- | Like `fixup` lives in `m` and the application `Step s`
fixupM' :: forall m s. Functor m => (DomNode -> Step s -> m { restore :: m Unit }) -> Prop m s
fixupM' f =
  Prop.mkFixup \node -> Rev.mkRevertibleM $ MationT \step ->
    (MationT.lift <<< _.restore) <$> f node step

