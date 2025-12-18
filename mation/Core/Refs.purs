
-- | This module defines a select handful of mutable reference types.
-- | Each reference type supports some combination of the following four operations:
-- |
-- | - *read*, meaning that the reference can be read from
-- | - *write*, meaning that the reference can be written to
-- | - *modify*, meaning that one can apply a function of type `a -> a` to the reference's value
-- | - *listen*, meaning that one can listen for changes to the reference
-- |
-- | In particular, the following types are defined:
-- |
-- | - `ReadWriteL`, which is a read/write/modify/listen reference
-- | - `ReadWrite`, which is read/write/modify
-- | - `Modify`, which is write/modify
-- | - `Write`, which is write-only
-- | - `ReadL`, which is read/listen
-- | - `Read`, which is read-only
-- | - `Nil`, which supports none of the four operations
-- |
-- | Each reference type also comes with a *focus* operation, which gives a way
-- | to form "subreferences" (or "virtual references") which point to only a part
-- | of the state of the original reference.
-- |
-- | There is a `DowncastRef` class which gives you a way of turning
-- | a given reference (such as some read/write/modify `ReadWrite` value) into a
-- | less-powerful version (eg, into a write-only `Write` value)
-- |
-- | Each of the four operations *read*, *write*, *modify*, and *listen* are
-- | reified as a typeclass (eg `ReadRef`) which is instantiated by all
-- | ref types which support that operation.
-- |
-- | Ref types are coupled to the `Effect` monad. In principle it's possible
-- | possible to replace `Effect` by some type parameter `m` (and in earlier
-- | versions of the library, this was the case). However, these ref types
-- | exist in particular for use as Mation capabilities, in which case `m`
-- | will always be instantiated to `Effect` (since the Mation runtime lives
-- | in `Effect`)
-- |
-- | ***
-- |
-- | Now I will say a some mathy stuff.
-- |
-- | There are four possible operations that this module's reference types
-- | can support: *read*, *write*, *modify*, and *listen*. For completeness,
-- | this module ought to support every possible combination and hence
-- | provide 2⁴ = 16 different reference types. But it only provides 7.
-- | What gives? The answer comes in two parts.
-- |
-- | For one, not every combination of *read*, *write*, *modify*, and *listen*
-- | needs to be present for completeness. Notice that if you have a ref
-- | that supports *modify*, then you can also use it to *write*, by calling
-- | the `modify` function with an `a -> a` of the form `const newValue`.
-- | This means that supplying both a modify-only ref and a modify/write ref
-- | would be redundant, as they are in effect equivalent.
-- |
-- | Redundancies like this mean that, considering only the three operations
-- | *read*, *write*, and *modify*, there are exactly 5 different types of refs
-- | (rather than 2³ = 8); they are:
-- |
-- | - read/write (which can also *modify*)
-- | - modify (which can also *write*)
-- | - write
-- | - read
-- | - nil
-- |
-- | Regarding the *listen* operation, the situation is different. The *listen*
-- | operation actually has *no* redundancies, so if we wanted this module to
-- | be complete we should define 5 × 2 = 10 different ref types (one each
-- | of the five listed above, plus another version of each which also
-- | supports *listen*).
-- |
-- | However, we make the assumption that if you want to *listen* to a ref then
-- | you probably also want to *read* it; accordinly we only
-- | define *listen* versions of ref types which support *read*.

module Mation.Core.Refs

  ( ReadWriteL
  , ReadWrite
  , Modify
  , Write
  , ReadL
  , Read
  , Nil
  , nil

  , class ReadRef
  , read
  , class WriteRef
  , write
  , class ModifyRef
  , modify
  , class ListenRef
  , onNextChange
  , onChange
  , onChange'

  , class MakeRef
  , makeWithSelf
  , make

  , class FocusRefWithLens
  , focusWithLens
  , class FocusRefWithSetter
  , focusWithSetter
  , class FocusRefWithGetter
  , focusWithGetter
  , class FocusRefWithOpFun
  , focusWithOpFun

  , class DowncastRef
  , downcast
  , downcastTo

  , sync
  ) where

import Mation.Core.Prelude

import Effect.Ref as ERef
import Data.Lens as Lens
import Data.Array as Array
import Data.Traversable (sequence_)


-- | Instantiated by reference types which support reading
class ReadRef ref where
  read :: forall m a. MonadEffect m => ref a -> m a

-- | Instantiated by reference types which support writing
class WriteRef ref where
  write :: forall m a. MonadEffect m => a -> ref a -> m Unit

-- | Instantiated by reference types which support the *modify* operation
class ModifyRef ref where
  modify :: forall m a. MonadEffect m => (a -> a) -> ref a -> m Unit

-- | Instantiated by reference types which support registering change
-- | listeners.
-- |
-- | Listeners are notified that an update occurred but *not* what the new
-- | value of the reference is. To find that, they must read the ref themselves.
-- |
-- | Listeners are removed after they are fired. To listen to every change of
-- | a `ListenRef` ref, a callsite must repeatedly re-attach its listener
-- | (or use `onChange`).
-- |
-- | `ReadRef` is a superclass of `ListenRef`. This is "artificial" in the
-- | sense that one cannot recover `read` from `onNextChange`. However, almost
-- | always if one wants to know when a ref value changes, then they also
-- | want to know what the value changes *to*; hence, it's appropriate to
-- | couple the two classes.
class ReadRef ref <= ListenRef ref where
  onNextChange :: forall m a. MonadUnliftEffect m => m Unit -> ref a -> m Unit

-- | Instances give a way to construct a reference type in some fixed monad `m`
--
-- This is the only way we allow a library user to create new references.
-- In principle we could supply constructors like `mkRead :: m a -> Read m a`.
-- But since we lack any laws for reference values, that could result
-- in the creation of "bad" references, such as a `Write a` which does
-- not write to a mutable location (but instead eg emits an event).
class MakeRef ref where

  -- | Create a reference which can close over itself
  --
  -- This is safe because using a reference in any way is a monadic action,
  -- and `ref a -> a` is not monadic. Hence the `ref a -> a` function cannot
  -- actually use the ref, it can only place the reference somewhere.
  makeWithSelf :: forall m a. MonadEffect m => (ref a -> a) -> m (ref a)

-- | Make a new reference from an initial value
make :: forall m ref a. MonadEffect m => MakeRef ref => a -> m (ref a)
make a = makeWithSelf (\_self -> a)

-- | Reference types who can `focus` with a `Lens'`
-- |
-- | There is a `FocusRefWithX` class for various choice
-- | of `X`. This is less pretty than having a single `FocusRefWith`
-- | (with instances like `FocusRefWith Lens' ReadWrite`), but
-- | for technical reasons that's not possible/desirable.
--
-- Namely, the technical reasons are these. Say we wanted a class
--
--   class FocusRefWith len ref where
--     focus :: forall m s a. len s a -> ref m s -> ref m a
--
-- This won't work. Purescript will reject instances
-- like `FocusRefWith Lens' ReadWrite` because Lens' is not a type
-- constructor.
--
-- In the future we might (see [1]) be able to fix this by adding a
-- fundep `ref -> len`. But then we can't have both instances
--
--   FocusRefWith Setter' Modify
--   FocusRefWith Lens' Modify
--
-- which is bad. This is bad because it makes stuff like the following
-- fail:
--
--   duplicate :: forall m ref a. FocusRefWith Lens' ref =>
--     Html m (ref a) -> Html m (ref (a /\ a))
--   duplicate html = fold
--     [ cmap (focusWithLens _1) html
--     , cmap (focusWithLens _2) html
--     ]
--
-- If Modify is only allowed the instance `FocusRefWith Setter' Modify`,
-- then trying to choose `ref ~ Modify` in the above will fail
-- at instance resolution.
--
-- [1]: <https://github.com/purescript/purescript/issues/4514>
class FocusRefWithLens ref where
  focusWithLens :: forall s a. Lens' s a -> ref s -> ref a

-- | Reference types who can `focus` with a `Setter'`
class FocusRefWithSetter ref where
  focusWithSetter :: forall s a. Setter' s a -> ref s -> ref a

-- | Reference types who can `focus` with a `Getter'`
class FocusRefWithGetter ref where
  focusWithGetter :: forall s a. Getter' s a -> ref s -> ref a

-- | Reference types who can `focus` with a function `a -> s`
class FocusRefWithOpFun ref where
  focusWithOpFun :: forall s a. (a -> s) -> ref s -> ref a



-- | An instance of `DowncastRef r1 r2` gives a way to transform a reference
-- | of type `r1` into a reference of type `r2`.
-- |
-- | Downcasting commutes with read/write/modify/listen operations; for
-- | instance, `ref # write v` is the same as `(downcast ref) # write v`.
-- |
-- | Every reference type can be downcast to itself.
-- |
-- | DowncastRefing forms a poset / thin category. In particular, this one:
-- |
-- | ```
-- | ReadWriteL ──────────────> ReadWrite ────> Modify ────> Write
-- |    │          downcast        │                           │
-- |    │                          │                           │
-- |    V                          V                           V
-- |  ReadL ────────────────────> Read ─────────────────────> Nil
-- | ```
class DowncastRef :: forall k. (k -> Type) -> (k -> Type) -> Constraint
class DowncastRef ref ref' where
  downcast :: forall a. ref a -> ref' a

-- | This is just `downcast` with its type parameter order reversed
-- |
-- | Useful for use with visible type applications: `downcastTo @(Read m)`
downcastTo :: forall @ref' @ref a. DowncastRef ref ref' => ref a -> ref' a
downcastTo = downcast



-- | Reference type with read/write/modify/listen capabilities
data ReadWriteL a = ReadWriteL (Effect a) (a -> Effect Unit) (Effect Unit -> Effect Unit)

instance ReadRef ReadWriteL where
  read (ReadWriteL readIt _ _) = liftEffect readIt

instance WriteRef ReadWriteL where
  write a (ReadWriteL _ writeIt _) = liftEffect (writeIt a)

instance ListenRef ReadWriteL where
  onNextChange f (ReadWriteL _ _ onNextChangeIt) =
    withRunInEffect \toEffect -> onNextChangeIt (toEffect f)

instance ModifyRef ReadWriteL where
  modify f (ReadWriteL readIt writeIt _) =
    liftEffect $
      readIt >>= (f >>> writeIt)
    -- This implementation is valid if we assume that at most one thread
    -- can have access to the ref at any given time.
    -- Because of Javascript's concurrency model, this should be a safe
    -- assumption to make.

instance MakeRef ReadWriteL where

  makeWithSelf :: forall m a. MonadEffect m => (ReadWriteL a -> a) -> m (ReadWriteL a)
  makeWithSelf init = liftEffect do

    listenersRef <- ERef.new []

    let
      upgrade :: ERef.Ref a -> ReadWriteL a
      upgrade valRef = let
        readIt = ERef.read valRef
        writeIt a = do
          -- Write value
          valRef # ERef.write a
          -- Notify & detach listeners
          listeners <- ERef.read listenersRef
          listenersRef # ERef.write []
          sequence_ listeners
        onNextChangeIt listener =
          listenersRef # ERef.modify_ (flip Array.snoc listener)
        in (ReadWriteL readIt writeIt onNextChangeIt)

    valRef <- ERef.newWithSelf \valRef -> init (upgrade valRef)
    pure (upgrade valRef)



-- | Given a `Lens' s a`, we can form a "subreference" of a `ReadWriteL m s`
-- |
-- | This "subreference" shares state with the original reference.
-- | When one is updated, both will see the change!
instance FocusRefWithLens ReadWriteL where
  focusWithLens len rwl =
    ReadWriteL
      (Lens.view len <$> read rwl)
      (\a -> rwl # modify (len .~ a))
      (\f -> rwl # onNextChange f)

instance DowncastRef ReadWriteL ReadWriteL where
  downcast = identity



-- | Reference type with read/write/modify capabilities
data ReadWrite a = ReadWrite (Effect a) (a -> Effect Unit)

instance ReadRef ReadWrite where
  read (ReadWrite readIt _) = liftEffect readIt

instance WriteRef ReadWrite where
  write a (ReadWrite _ writeIt) = liftEffect (writeIt a)

instance ModifyRef ReadWrite where
  modify f (ReadWrite readIt writeIt) =
    liftEffect $
      readIt >>= (f >>> writeIt)

instance MakeRef ReadWrite where
  makeWithSelf :: forall m a. MonadEffect m => (ReadWrite a -> a) -> m (ReadWrite a)
  makeWithSelf init =
    downcast <$> (makeWithSelf (init <<< downcast) :: m (ReadWriteL a))

-- | Use a `Lens'` to construct a subreference
instance FocusRefWithLens ReadWrite where
  focusWithLens len rw =
    ReadWrite
      ((_ ^. len) <$> read rw)
      (\a -> rw # modify (len .~ a))

instance DowncastRef ReadWrite ReadWrite where
  downcast = identity

instance DowncastRef ReadWriteL ReadWrite where
  downcast (ReadWriteL readIt writeIt _) = ReadWrite readIt writeIt



-- | Reference type with modify capabilities
newtype Modify a = Modify ((a -> a) -> Effect Unit)

instance WriteRef Modify where
  write a (Modify modify) = liftEffect (modify (const a))

instance ModifyRef Modify where
  modify f (Modify modify) = liftEffect (modify f)

instance MakeRef Modify where
  makeWithSelf :: forall m a. MonadEffect m => (Modify a -> a) -> m (Modify a)
  makeWithSelf init =
    downcast <$> (makeWithSelf (init <<< downcast) :: m (ReadWriteL a))

-- | Use a `Setter'` to construct a subreference
instance FocusRefWithSetter Modify where
  focusWithSetter len (Modify modifyIt) =
    Modify (\f -> modifyIt (len %~ f))

instance FocusRefWithLens Modify where
  focusWithLens len = focusWithSetter len

instance DowncastRef Modify Modify where
  downcast = identity

instance DowncastRef ReadWriteL Modify where
  downcast rwl = Modify (\f -> rwl # modify f)

instance DowncastRef ReadWrite Modify where
  downcast rw = Modify (\f -> rw # modify f)



-- | Reference type with write capabilities
newtype Write a = Write (a -> Effect Unit)

instance WriteRef Write where
  write a (Write writeIt) = liftEffect (writeIt a)

instance MakeRef Write where
  makeWithSelf :: forall m a. MonadEffect m => (Write a -> a) -> m (Write a)
  makeWithSelf init =
    downcast <$> (makeWithSelf (init <<< downcast) :: m (ReadWriteL a))

-- | Use a function to construct a subreference
instance FocusRefWithOpFun Write where
  focusWithOpFun up (Write writeIt) =
    Write (\a -> writeIt (up a))

instance DowncastRef Write Write where
  downcast = identity

instance DowncastRef ReadWriteL Write where
  downcast (ReadWriteL _ writeIt _) = Write writeIt

instance DowncastRef ReadWrite Write where
  downcast (ReadWrite _ writeIt) = Write writeIt

instance DowncastRef Modify Write where
  downcast (Modify modifyIt) = Write (modifyIt <<< const)



-- | Reference type with read/listen capabilities
data ReadL a = ReadL (Effect a) (Effect Unit -> Effect Unit)

instance ReadRef ReadL where
  read (ReadL readIt _) = liftEffect readIt

instance ListenRef ReadL where
  onNextChange f (ReadL _ onNextChangeIt) =
    withRunInEffect \toEffect -> onNextChangeIt (toEffect f)

instance MakeRef ReadL where
  makeWithSelf :: forall m a. MonadEffect m => (ReadL a -> a) -> m (ReadL a)
  makeWithSelf init =
    downcast <$> (makeWithSelf (init <<< downcast) :: m (ReadWriteL a))

-- | Use a `Getter'` to construct a subreference
instance FocusRefWithGetter ReadL where
  focusWithGetter len rl =
    ReadL
      (Lens.view len <$> read rl)
      (\f -> rl # onNextChange f)

instance FocusRefWithLens ReadL where
  focusWithLens len = focusWithGetter len

instance DowncastRef ReadL ReadL where
  downcast = identity

instance DowncastRef ReadWriteL ReadL where
  downcast (ReadWriteL readIt _ onNextChangeIt) = ReadL readIt onNextChangeIt



-- | Reference type with read capabilities
newtype Read a = Read (Effect a)

instance ReadRef Read where
  read (Read readIt) = liftEffect readIt

instance MakeRef Read where
  makeWithSelf :: forall m a. MonadEffect m => (Read a -> a) -> m (Read a)
  makeWithSelf init =
    downcast <$> (makeWithSelf (init <<< downcast) :: m (ReadWriteL a))

-- | Use a `Getter'` to construct a subreference
instance FocusRefWithGetter Read where
  focusWithGetter len (Read readIt) =
    Read (Lens.view len <$> readIt)

instance FocusRefWithLens Read where
  focusWithLens len = focusWithGetter len

instance DowncastRef Read Read where
  downcast = identity

instance DowncastRef ReadWriteL Read where
  downcast (ReadWriteL readIt _ _) = Read readIt

instance DowncastRef ReadWrite Read where
  downcast (ReadWrite readIt _) = Read readIt

instance DowncastRef ReadL Read where
  downcast (ReadL readIt _) = Read readIt



-- | Reference type with no capabilities
data Nil :: forall k. k -> Type
data Nil a = Nil

nil :: forall a. Nil a
nil = Nil

instance MakeRef Nil where
  makeWithSelf _ = pure Nil

instance FocusRefWithLens Nil where
  focusWithLens _ _ = Nil

instance FocusRefWithSetter Nil where
  focusWithSetter _ _ = Nil

instance FocusRefWithGetter Nil where
  focusWithGetter _ _ = Nil

instance FocusRefWithOpFun Nil where
  focusWithOpFun _ _ = Nil

instance DowncastRef Nil Nil where
  downcast _ = Nil

instance DowncastRef ReadWriteL Nil where
  downcast _ = Nil

instance DowncastRef ReadWrite Nil where
  downcast _ = Nil

instance DowncastRef Modify Nil where
  downcast _ = Nil

instance DowncastRef Write Nil where
  downcast _ = Nil

instance DowncastRef ReadL Nil where
  downcast _ = Nil

instance DowncastRef Read Nil where
  downcast _ = Nil


---


-- | Modify the value in a ref. Returns the new value
modify' :: forall m ref a. MonadEffect m => ReadRef ref => WriteRef ref =>
  (a -> a) -> ref a -> m a
modify' f ref = do
  val <- read ref
  let val' = f val
  write val' ref
  pure val'

-- | Watch value changes of a read/listen ref
-- |
-- | This differs from `onNextChange` in two ways:
-- | - `onChange` supplies the new ref value
-- | - `onChange` keeps the listener attached to the ref. (`onNextChange` is one-shot)
onChange :: forall m ref a. MonadUnliftEffect m => ListenRef ref =>
  (a -> m Unit) -> ref a -> m Unit
onChange f ref =
  ref # onNextChange do
    newVal <- read ref
    f newVal
    onChange f ref

-- | Like `onChange`, but provides both the old and new values
onChange' :: forall m ref a. MonadUnliftEffect m => ListenRef ref =>
  ({ old :: a, new :: a } -> m Unit) -> ref a -> m Unit
onChange' f ref = do
  oldValRef <- read ref >>= (make :: a -> m (ReadWrite a))
  ref # onChange \newVal -> do
    oldVal <- read oldValRef
    oldValRef # write newVal
    f { old: oldVal, new: newVal }


-- | Create a two-way binding with a read/write/listen ref
-- |
-- | For instance,
-- |
-- | ```
-- | do
-- |   (ref :: ReadWrite Int) <- make 10
-- |
-- |   writeToRef <- ref # sync \(newVal :: Int) ->
-- |     -- Called every time `ref` changes value
-- |     Console.log newVal
-- |
-- |   writeToRef 20
-- | ```
-- |
-- | Differs from creating a two-way binding with `onChange` and `push` in
-- | that bindings created with `sync` will not self-invoke.
-- |
-- | ***
-- |
-- | A first attempt at creating a two-way sync is to use `onChange` for
-- | one direction of the sync (or `onChange'`) and `write` for the other
-- | direction of the sync.
-- |
-- | The trouble with this is that it causes unnecessary round-tripping.
-- | When the non-`ref` state changes, it will invoke `write`, which
-- | will invoke the `onChange` listener, which is redundant. Likewise,
-- | when the `ref` state changes, it will invoke the `onChange` listener,
-- | which will update the non-`ref` state, which will invoke `write`,
-- | which is redundant.
-- |
-- | In fact, if one uses `onChange` this should cause an infinite loop.
-- | (The other option is to use `onChange'` and using `==` to test in
-- | the listener for if the state has actually changed)
-- |
-- | The value add of `sync` is that it avoids this round-tripping. You
-- | pass in a `listen :: a -> Effect Unit` (as you would to `onChange`)
-- | and recieve a `tell :: a -> Effect Unit` (akin to `write`) and
-- | when they are invoked they will avoid invoking the other.
sync :: forall m ref a. MonadUnliftEffect m => ListenRef ref => WriteRef ref =>
  (a -> m Unit) -> ref a -> m (a -> m Unit)
sync onPush ref = do
  (rDont :: ReadWrite _) <- make false
  ref # onChange \newVal -> sync'd rDont (onPush newVal)
  pure \val -> sync'd rDont (ref # write val)

  where

  sync'd rDont action = do
    read rDont >>= case _ of
      true -> pure unit
      false -> do
        rDont # write true
        action
        rDont # write false

{- TODO

-- | Transforms a `Step s` stepper into a "buffered stepper" which
-- | stages state updates `s -> s` before applying them all at once.
-- |
-- | Intended to be used with event handlers like
-- |
-- | ```purs
-- | onClick \step -> do
-- |   { stage, apply } <- M.toBuffered step
-- |   ...
-- | ```
-- |
-- | It's tempting to instead write
-- |
-- | ```purs
-- | onClick $ M.toBuffered >=> \{ stage, apply } -> ...`
-- | ```
-- |
-- | but this makes the typesystem choke up due to `Step`
-- | containing a `forall`
toBuffered :: forall m s.
  MonadEffect m => Step s -> m (BufferedStep m s)
toBuffered step = liftEffect do
  buf <- liftEffect $ Ref.new identity
  pure
    { stage: \g -> liftEffect do
        Ref.modify_ (_ >>> g) buf
    , apply: do
        liftEffect (Ref.read buf) >>= step
        liftEffect $ Ref.write identity buf
    }

type BufferedStep m s =
    -- | Add a state update to the buffer
  { stage :: (s -> s) -> m Unit
    -- | Apply all staged updates to the state
  , apply :: m Unit
  }


-}
