
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
-- | There is a `Downcast` class which gives you a way of turning
-- | a given reference (such as some read/write/modify `ReadWrite` value) into a
-- | less-powerful version (eg, into a write-only `Write` value)
-- |
-- | Each of the four operations *read*, *write*, *modify*, and *listen* are
-- | reified as a typeclass (eg `CanRead`) which is instantiated by all
-- | ref types which support that operation. The intent behind this is *not*
-- | for library users to write components which are polymorphic over
-- | the ref type! Rather, the intent is that if/when a library user
-- | refactors from a `ReadWrite` to a `Read`, everything "just works".
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
  ( class CanRead
  , read
  , class CanWrite
  , write
  , class CanModify
  , modify
  , class CanListen
  , onNextChange
  , onChange
  , onChange'
  , class CanMake
  , makeWithSelf
  , make
  , class CanFocusWithLens
  , focusWithLens
  , class CanFocusWithSetter
  , focusWithSetter
  , class CanFocusWithGetter
  , focusWithGetter
  , class Downcast
  , downcast
  , downcastTo
  , ReadWriteL
  , ReadWriteL'
  , focusReadWriteL
  , hoistReadWriteL
  , ReadWrite
  , ReadWrite'
  , focusReadWrite
  , hoistReadWrite
  , Modify
  , Modify'
  , focusModify
  , hoistModify
  , Write
  , Write'
  , focusWrite
  , hoistWrite
  , ReadL
  , ReadL'
  , focusReadL
  , hoistReadL
  , Read
  , Read'
  , focusRead
  , hoistRead
  , Nil
  , Nil'
  , mkNil
  , focusNil
  , hoistNil

  , sync
  ) where

import Mation.Core.Prelude

import Effect.Ref as ERef
import Data.Lens as Lens
import Data.Lens (ALens')
import Data.Array as Array
import Data.Traversable (sequence_)


-- | Instantiated by reference types which support reading
class CanRead :: forall k. (k -> Type) -> (k -> Type) -> Constraint
class CanRead m ref | ref -> m where
  read :: forall a. ref a -> m a

-- | Instantiated by reference types which support writing
class CanWrite m ref | ref -> m where
  write :: forall a. a -> ref a -> m Unit

-- | Instantiated by reference types which support the *modify* operation
class CanModify m ref | ref -> m where
  modify :: forall a. (a -> a) -> ref a -> m Unit

-- | Instantiated by reference types which support registering change
-- | listeners.
-- |
-- | Listeners are notified that an update occurred but *not* what the new
-- | value of the reference is. To find that, they must read the ref themselves.
-- |
-- | Listeners are removed after they are fired. To listen to every change of
-- | a `CanListen` ref, a callsite must repeatedly re-attach its listener
-- | (or use `onChange`).
class CanListen :: forall k. (Type -> Type) -> (k -> Type) -> Constraint
class CanListen m ref | ref -> m where
  onNextChange :: forall a. m Unit -> ref a -> m Unit

-- | Instances give a way to construct a reference type in some fixed monad `m`
--
-- This is the only way we allow a library user to create new references.
-- In principle we could supply constructors like `mkRead :: m a -> Read m a`.
-- But since we lack any laws for reference values, that could result
-- in the creation of some "bad" references like a `Write a` which does
-- not write to a mutable location but rather appends to a write-log.
class CanMake m ref where

  -- | Create a reference which can close over itself
  --
  -- This is safe because using a reference in any way is a monadic action,
  -- and `ref a -> a` is not monadic. Hence the `ref a -> a` function cannot
  -- actually use the ref, it can only place the reference somewhere.
  makeWithSelf :: forall a. (ref a -> a) -> m (ref a)

-- | Make a new reference from an initial value
make :: forall m ref a. CanMake m ref => a -> m (ref a)
make a = makeWithSelf (\_self -> a)

-- | Reference types who can `focus` with a `Lens'`
-- |
-- | Ideally there would just be a `CanFocus` class, so one
-- | can write `CanFocus Lens' ref`. But because of the way `Lens'`
-- | is defined, there's no good way (that I know of) to do this.
-- |
-- | Instead we have a `CanFocusWithX` class for each optic
-- | type `X` which can be used to focus >1 ref types.
class CanFocusWithLens ref where
  focusWithLens :: forall s a. Lens' s a -> ref s -> ref a

-- | Reference types who can `focus` with a `Setter'`
class CanFocusWithSetter ref where
  focusWithSetter :: forall s a. Setter' s a -> ref s -> ref a

-- | Reference types who can `focus` with a `Getter'`
class CanFocusWithGetter ref where
  focusWithGetter :: forall s a. Getter' s a -> ref s -> ref a

class CanFocus lenSA ref s a | ref -> lenSA where
  focus :: lenSA -> ref s -> ref a

focusReadWriteL' :: forall m s a. Bind m =>
  ALens' s a -> ReadWriteL m s -> ReadWriteL m a
focusReadWriteL' alen rwl =
  let len = cloneLens alen in
  ReadWriteL
    (Lens.view len <$> read rwl)
    (\a -> rwl # modify (len .~ a))
    (\f -> rwl # onNextChange f)

instance Bind m => CanFocus (ALens' s a) (ReadWriteL m) s a where
  focus len = focusReadWriteL' len

-- | An instance of `Downcast r1 r2` gives a way to transform a reference
-- | of type `r1` into a reference of type `r2`.
-- |
-- | Downcasting commutes with read/write/modify/listen operations; for
-- | instance, `ref # write v` is the same as `(downcast ref) # write v`.
-- |
-- | Every reference type can be downcast to itself.
-- |
-- | Downcasting forms a poset / thin category. In particular, this one:
-- |
-- | ```
-- | ReadWriteL ──────────────> ReadWrite ────> Modify ────> Write
-- |    │          downcast        │                           │
-- |    │                          │                           │
-- |    │                          │                           │
-- |    V                          V                           V
-- |  ReadL ────────────────────> Read ─────────────────────> Nil
-- | ```
class Downcast :: forall k. (k -> Type) -> (k -> Type) -> Constraint
class Downcast ref ref' where
  downcast :: forall a. ref a -> ref' a

-- | This is just `downcast` with its type parameter order reversed
-- |
-- | Useful for use with visible type applications: `downcastTo @(Read m)`
downcastTo :: forall @ref' @ref a. Downcast ref ref' => ref a -> ref' a
downcastTo = downcast



-- | Reference type with read/write/modify/listen capabilities
data ReadWriteL m a = ReadWriteL (m a) (a -> m Unit) (m Unit -> m Unit)

type ReadWriteL' = ReadWriteL Effect

instance CanRead m (ReadWriteL m) where
  read (ReadWriteL readIt _ _) = readIt

instance CanWrite m (ReadWriteL m) where
  write a (ReadWriteL _ writeIt _) = writeIt a

instance CanListen m (ReadWriteL m) where
  onNextChange f (ReadWriteL _ _ onNextChangeIt) = onNextChangeIt f

instance Bind m => CanModify m (ReadWriteL m) where
  modify f (ReadWriteL readIt writeIt _) =
    readIt >>= (f >>> writeIt)
    -- This implementation is valid if we assume that at most one thread
    -- can have access to the ref at any given time.
    -- Because of Javascript's concurrency model, this should be a safe
    -- assumption to make.

instance CanMake Effect (ReadWriteL Effect) where

  makeWithSelf :: forall a. (ReadWriteL Effect a -> a) -> Effect (ReadWriteL Effect a)
  makeWithSelf init = do

    listenersRef <- ERef.new []

    let
      upgrade :: ERef.Ref a -> ReadWriteL Effect a
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
focusReadWriteL :: forall m s a. Bind m =>
  Lens' s a -> ReadWriteL m s -> ReadWriteL m a
focusReadWriteL len rwl =
  ReadWriteL
    (Lens.view len <$> read rwl)
    (\a -> rwl # modify (len .~ a))
    (\f -> rwl # onNextChange f)

instance Bind m => CanFocusWithLens (ReadWriteL m) where
  focusWithLens = focusReadWriteL

-- | Lift a monad isomorphism to a mapping of `ReadWriteL`
-- |
-- | An entire isomorphism is required to hoist the ref listeners
hoistReadWriteL :: forall m n. (m ~> n) -> (n ~> m) -> (ReadWriteL m ~> ReadWriteL n)
hoistReadWriteL toN fromN (ReadWriteL readIt writeIt onNextChangeIt) =
    ReadWriteL (toN readIt) (toN <$> writeIt) (fromN >>> onNextChangeIt >>> toN)

instance Downcast (ReadWriteL m) (ReadWriteL m) where
  downcast = identity



-- | Reference type with read/write/modify capabilities
data ReadWrite m a = ReadWrite (m a) (a -> m Unit)

type ReadWrite' = ReadWrite Effect

instance CanRead m (ReadWrite m) where
  read (ReadWrite readIt _) = readIt

instance CanWrite m (ReadWrite m) where
  write a (ReadWrite _ writeIt) = writeIt a

instance Bind m => CanModify m (ReadWrite m) where
  modify f (ReadWrite readIt writeIt) =
    readIt >>= (f >>> writeIt)

instance CanMake Effect (ReadWrite Effect) where
  makeWithSelf init =
    downcast <$> makeWithSelf @_ @(ReadWriteL Effect) (init <<< downcast)

-- | Use a `Lens'` to construct a subreference
focusReadWrite :: forall m s a. Bind m =>
  Lens' s a -> ReadWrite m s -> ReadWrite m a
focusReadWrite len rw =
  ReadWrite
    ((_ ^. len) <$> read rw)
    (\a -> rw # modify (len .~ a))

instance Bind m => CanFocusWithLens (ReadWrite m) where
  focusWithLens = focusReadWrite

-- | Lift a monad morphism to a mapping of `ReadWrite`
hoistReadWrite :: forall m n. (m ~> n) -> (ReadWrite m ~> ReadWrite n)
hoistReadWrite toN (ReadWrite readIt writeIt) =
    ReadWrite (toN readIt) (toN <$> writeIt)

instance Downcast (ReadWrite m) (ReadWrite m) where
  downcast = identity

instance Downcast (ReadWriteL m) (ReadWrite m) where
  downcast (ReadWriteL readIt writeIt _) = ReadWrite readIt writeIt



-- | Reference type with modify capabilities
newtype Modify m a = Modify ((a -> a) -> m Unit)

type Modify' = Modify Effect

instance CanWrite m (Modify m) where
  write a (Modify modify) = modify (const a)

instance CanModify m (Modify m) where
  modify f (Modify modify) = modify f

instance CanMake Effect (Modify Effect) where
  makeWithSelf init =
    downcast <$> makeWithSelf @_ @(ReadWriteL Effect) (init <<< downcast)

-- | Use a `Setter'` to construct a subreference
focusModify :: forall m s a.
  Setter' s a -> Modify m s -> Modify m a
focusModify len (Modify modifyIt) =
  Modify (\f -> modifyIt (len %~ f))

instance CanFocusWithLens (Modify m) where
  focusWithLens len = focusModify len

instance CanFocusWithSetter (Modify m) where
  focusWithSetter = focusModify

-- | Lift a monad morphism to a mapping of `Modify`
hoistModify :: forall m n. (m ~> n) -> (Modify m ~> Modify n)
hoistModify toN (Modify modifyIt) =
    Modify (modifyIt >>> toN)

instance Downcast (Modify m) (Modify m) where
  downcast = identity

instance Bind m => Downcast (ReadWriteL m) (Modify m) where
  downcast rwl = Modify (\f -> rwl # modify f)

instance Bind m => Downcast (ReadWrite m) (Modify m) where
  downcast rw = Modify (\f -> rw # modify f)



-- | Reference type with write capabilities
newtype Write m a = Write (a -> m Unit)

type Write' = Write Effect

instance CanWrite m (Write m) where
  write a (Write write) = write a

instance CanMake Effect (Write Effect) where
  makeWithSelf init =
    downcast <$> makeWithSelf @_ @(ReadWriteL Effect) (init <<< downcast)

-- | Use a function to construct a subreference
focusWrite :: forall m s a.
  (a -> s) -> Write m s -> Write m a
focusWrite up (Write writeIt) =
  Write (\a -> writeIt (up a))

-- | Lift a monad morphism to a mapping of `Write`
hoistWrite :: forall m n. (m ~> n) -> (Write m ~> Write n)
hoistWrite toN (Write writeIt) =
    Write (writeIt >>> toN)

instance Downcast (Write m) (Write m) where
  downcast = identity

instance Downcast (ReadWriteL m) (Write m) where
  downcast (ReadWriteL _ writeIt _) = Write writeIt

instance Downcast (ReadWrite m) (Write m) where
  downcast (ReadWrite _ writeIt) = Write writeIt

instance Downcast (Modify m) (Write m) where
  downcast (Modify modifyIt) = Write (modifyIt <<< const)



-- | Reference type with read/listen capabilities
data ReadL m a = ReadL (m a) (m Unit -> m Unit)

type ReadL' = ReadL Effect

instance CanRead m (ReadL m) where
  read (ReadL readIt _) = readIt

instance CanListen m (ReadL m) where
  onNextChange f (ReadL _ onNextChangeIt) = onNextChangeIt f

instance CanMake Effect (ReadL Effect) where
  makeWithSelf init =
    downcast <$> makeWithSelf @_ @(ReadWriteL Effect) (init <<< downcast)

-- | Use a `Getter'` to construct a subreference
focusReadL :: forall m s a. Functor m =>
  Getter' s a -> ReadL m s -> ReadL m a
focusReadL len rl =
  ReadL
    (Lens.view len <$> read rl)
    (\f -> rl # onNextChange f)

instance Functor m => CanFocusWithLens (ReadL m) where
  focusWithLens len = focusReadL len

instance Functor m => CanFocusWithGetter (ReadL m) where
  focusWithGetter len = focusReadL len

-- | Lift a monad isomorphism to a mapping of `ReadL`
-- |
-- | An entire isomorphism is required to hoist the ref listeners
hoistReadL :: forall m n. (m ~> n) -> (n ~> m) -> (ReadL m ~> ReadL n)
hoistReadL toN fromN (ReadL readIt onNextChangeIt) =
    ReadL (toN readIt) (fromN >>> onNextChangeIt >>> toN)

instance Downcast (ReadL m) (ReadL m) where
  downcast = identity

instance Downcast (ReadWriteL m) (ReadL m) where
  downcast (ReadWriteL readIt _ onNextChangeIt) = ReadL readIt onNextChangeIt



-- | Reference type with read capabilities
newtype Read :: forall k. (k -> Type) -> k -> Type
newtype Read m a = Read (m a)

type Read' = Read Effect

instance CanRead m (Read m) where
  read (Read readIt) = readIt

instance CanMake Effect (Read Effect) where
  makeWithSelf init =
    downcast <$> makeWithSelf @_ @(ReadWriteL Effect) (init <<< downcast)

-- | Use a `Getter'` to construct a subreference
focusRead :: forall m s a. Functor m =>
  Getter' s a -> Read m s -> Read m a
focusRead len (Read readIt) =
  Read (Lens.view len <$> readIt)

instance Functor m => CanFocusWithLens (Read m) where
  focusWithLens len = focusRead len

instance Functor m => CanFocusWithGetter (Read m) where
  focusWithGetter len = focusRead len

-- | Lift a monad morphism to a mapping of `Read`
hoistRead :: forall m n. (m ~> n) -> (Read m ~> Read n)
hoistRead toN (Read readIt) =
    Read (toN readIt)

instance Downcast (Read m) (Read m) where
  downcast = identity

instance Downcast (ReadWriteL m) (Read m) where
  downcast (ReadWriteL readIt _ _) = Read readIt

instance Downcast (ReadWrite m) (Read m) where
  downcast (ReadWrite readIt _) = Read readIt

instance Downcast (ReadL m) (Read m) where
  downcast (ReadL readIt _) = Read readIt



-- | Reference type with no capabilities
data Nil :: forall k1 k2. k1 -> k2 -> Type
data Nil m a = Nil

type Nil' :: forall k. k -> Type
type Nil' = Nil Effect

mkNil :: forall m a. Nil m a
mkNil = Nil

instance Applicative m => CanMake m (Nil m) where
  makeWithSelf _ = pure Nil

-- | Construct a subreference
focusNil :: forall m s a. Nil m s -> Nil m a
focusNil Nil = Nil

instance CanFocusWithLens (Nil m) where
  focusWithLens _ _ = Nil

instance CanFocusWithSetter (Nil m) where
  focusWithSetter _ _ = Nil

hoistNil :: forall m n. Nil m ~> Nil n
hoistNil Nil = Nil

instance Downcast (Nil m) (Nil m) where
  downcast _ = Nil

instance Downcast (ReadWriteL m) (Nil m) where
  downcast _ = Nil

instance Downcast (ReadWrite m) (Nil m) where
  downcast _ = Nil

instance Downcast (Modify m) (Nil m) where
  downcast _ = Nil

instance Downcast (Write m) (Nil m) where
  downcast _ = Nil

instance Downcast (ReadL m) (Nil m) where
  downcast _ = Nil

instance Downcast (Read m) (Nil m) where
  downcast _ = Nil


---


-- | Modify the value in a ref. Returns the new value
modify' :: forall m ref a. Monad m => CanRead m ref => CanWrite m ref =>
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
onChange :: forall m ref a. Bind m => CanListen m ref => CanRead m ref =>
  (a -> m Unit) -> ref a -> m Unit
onChange f ref =
  ref # onNextChange do
    newVal <- read ref
    f newVal
    onChange f ref

-- | Like `onChange`, but provides both the old and new values
onChange' :: forall m ref a. Bind m => CanListen m ref => CanRead m ref => CanWrite m ref => CanMake m ref =>
  ({ old :: a, new :: a } -> m Unit) -> ref a -> m Unit
onChange' f ref = do
  oldValRef <- read ref >>= (make :: a -> m (ref a))
  ref # onChange \newVal -> do
    oldVal <- read oldValRef
    oldValRef # write newVal
    f { old: oldVal, new: newVal }


-- | Create a two-way binding with a read/write ref
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
sync :: forall ref a. CanRead Effect ref => CanWrite Effect ref => CanListen Effect ref =>
  (a -> Effect Unit) -> ref a -> Effect (a -> Effect Unit)
sync onPush ref = do
  (rDont :: ReadWrite Effect _) <- make false
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