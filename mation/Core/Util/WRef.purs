
-- | Watchable references
module Mation.Core.Util.WRef where

import Mation.Core.Prelude


-- | A `WRef a` is a mutable reference to a value of type `s`
-- |
-- | The `WRef` API is very similar to the `Effect.Ref` API.
-- | However, `WRef`s are more powerful, providing affordances
-- | like `onChange` and `mkView` that `Ref` does not.
-- |
-- | Remarks:
-- |
-- | - Most of the functions in the `WRef` API take the `WRef`
-- |   as their final parameter. This makes the functions
-- |   convenient to use as and with combinators, but annoying
-- |   to use on their own, as in
-- |
-- |   ```
-- |   WRef.modify (\oldValue -> someReallyLongVeryLargeExpression oldValue) myRef
-- |   ```
-- |
-- |   To remedy this, use `#`:
-- |
-- |   ```
-- |   myRef # WRef.modify \oldValue -> someReallyLongVeryLargeExpression oldValue
-- |   ```
-- |
-- | - Credit where credit's due, portions of the `WRef` API and
-- |   implementation come directly from `Effect.Ref`
-- |
-- | - The name `WRef` initially was meant to stand for "waitable ref", as
-- |   in a reference on which you can wait for a state change. At
-- |   this point, though, the `WRef` API provides more power than just
-- |   waiting.
foreign import data WRef :: Type -> Type

-- | Create a new `WRef`
make :: forall a. a -> Effect (WRef a)
make = make_f <<< const

-- | Create a new `WRef` with a value which can close over the `WRef
make' :: forall a. (WRef a -> a) -> Effect (WRef a)
make' = make_f

foreign import make_f :: forall a. (WRef a -> a) -> Effect (WRef a)

-- | Get the value out of a `WRef`
foreign import read :: forall a. WRef a -> Effect a

-- | Put a new value into a `WRef`
foreign import write :: forall a. a -> WRef a -> Effect Unit

-- | Modify the value in a `WRef`
modify :: forall a. (a -> a) -> WRef a -> Effect Unit
modify f = void <<< modify' f

-- | Modify the value in a `WRef`. Returns the new value
modify' :: forall a. (a -> a) -> WRef a -> Effect a
modify' f ref = do
  val <- read ref
  let val' = f val
  write val' ref
  pure val'


-- | Await a value change
-- |
-- | The provided callback will only be called once. If you want to
-- | continue waiting for value changes after the next one, you must
-- | call `nextChange` again (or use another API method)
-- |
-- | Change listeners are executed in the order that they were attached
foreign import nextChange :: forall a. Effect Unit -> WRef a -> Effect Unit

-- | Call a callback every time the value changes.
-- | Provides the callback with the new value.
-- |
-- | Remarks:
-- |
-- | - A `WRef` "changing" doesn't mean its value is necessarily
-- |   actually any different!
-- |
-- | - A callback provided to `onChange` is not guaranteed to see every
-- |   single state update. If one change listener modifies the state
-- |   before another change listener is invoked, the second listener
-- |   will recieve the doubly-modified state.
onChange :: forall a. (a -> Effect Unit) -> WRef a -> Effect Unit
onChange f ref =
  ref # nextChange do
    newVal <- read ref
    f newVal
    onChange f ref

-- | Like `onChange`, but provides both the old and new values
onChange' :: forall a. ({ old :: a, new :: a } -> Effect Unit) -> WRef a -> Effect Unit
onChange' f ref = do
  oldRef <- read ref >>= make
  ref # onChange \newVal -> do
    oldVal <- read oldRef
    write newVal oldRef
    f { old: oldVal, new: newVal }


-- | Create a `WRef` that acts as a view into a part of another `WRef`.
-- | The two `WRef`s will be two-way synchronized: if one changes, the
-- | other will update accordingly.
mkView :: forall a b. Lens' a b -> WRef a -> WRef b
mkView len = mkView_f { getter, setter }
  where
  getter = \a -> a ^. len
  setter = \b' a -> a # len .~ b'

foreign import mkView_f ::
  forall a b.
  { getter :: a -> b
  , setter :: b -> a -> a
  } -> WRef a -> WRef b