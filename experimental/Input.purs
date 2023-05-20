module Mation.Experimental.Input where

import Mation.Core.Prelude

import Data.Lens.Iso (Iso', re)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Either (either)
import Data.String.Pattern (Pattern (..)) as Str
import Data.String.Common (split) as Str
import Data.List (List (..))
import Data.List as List
import Data.Array as Array

import Mation (Html, Prop)
import Mation as M
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Core.Util.UnsureEq (class UnsureEq)
import Mation.Experimental.Input.Reading (Reading)
import Mation.Experimental.Input.Reading as R
import Mation.Experimental.Opts (Opts, def)


-- | Represents an input specification. Type parameters:
-- |
-- | - `t` -- The type the input produces
-- |
-- | - `model` -- The model underlying the input
-- |
-- | - `err` -- Type of read errors for the input.
-- |
-- |   If the input has no invalid states, leave this as a universal.
-- |   Doing so will allow for use of `readUnfailable`, which allows
-- |   reading the input without handling any error conditions.
-- |   (If having `err` be universal causes issues with the type system,
-- |   you can monomorphize to `err ~ Void` via `totalize` and later
-- |   re-polymorphize via `partialize`)
-- |
-- | Laws:
-- |
-- | - `isRight (read empty)`
-- | - `isRight (read (write x))` implies `read (write x) == Right x`
newtype InputSpec model err t = InputSpec

      -- | Write a value to the input
  { write :: t -> model

      -- | Attempt to read a value from the input
  , read :: model -> Either err t

      -- | Render the input, possibly with errors
      --
      -- One might think that accepting the errors as a parameter is redundant: we
      -- have 'read' which produces the errors; couldn't 'render' just call that to
      -- fetch them?
      --
      -- The trouble of doing that is as follows. A spec defining 'render' to
      -- enumerate its errors by calling 'read' will only be ware of errors that *it*
      -- cares about. If this spec is modified, eg via combinators, to add new error
      -- conditions, they will not be rendered. Parameterizing 'render' by 'Maybe err'
      -- fixes this, because it allows 'render' to be called by a third party (ie, not
      -- a spec itself) which uses the final spec to compute errors.
  , render :: Maybe err -> model -> Html Effect model

      -- | Empty input
  , empty :: model

  }


mkInputSpec :: forall model err t.
  { write :: t -> model
  , read :: model -> Either err t
  , render :: Maybe err -> model -> Html Effect model
  , empty :: model
  } -> InputSpec model err t
mkInputSpec = InputSpec

-- | Make an `InputSpec` keeping `err` open
mkUnfailableInputSpec :: forall model err t.
  { write :: t -> model
  , read :: model -> Either err t
  , render :: model -> Html Effect model
  , empty :: model
  } -> InputSpec model err t
mkUnfailableInputSpec spec =
  InputSpec $ spec
    { render = const spec.render
    }


-- | State for an input component whose model type is 'model'.
-- |
-- | The state stored is more than just 'model' itself because we track
-- | extra information on inputs like perturbation.
--
-- nb. The possibility was explored of having inputs track their own
-- perturbation state. It worked, and obviated the need for 'InputState',
-- but it was decided that overall the API was worse.
newtype InputState model = InputState
  { perturbed :: Boolean
  , model :: model
  }

derive instance Newtype (InputState model) _
derive newtype instance UnsureEq model => UnsureEq (InputState model)


-- | Empty state for an input
empty :: forall model err t. InputSpec model err t -> InputState model
empty (InputSpec spec) = InputState { model: spec.empty, perturbed: false }


-- | Initialize an input with a given value
valued :: forall model err t. InputSpec model err t -> t -> InputState model
valued (InputSpec spec) v = InputState { model: spec.write v, perturbed: false }


-- | Read the value out of a `Void`-errored input
readUnfailable :: forall model t. InputSpec model Void t -> InputState model -> t
readUnfailable (InputSpec spec) (InputState { model }) = spec.read model # either absurd identity


-- | Read the value out of an input embedded within some model (called `supermodel`)
readAt :: forall supermodel model err t.
  Lens' supermodel (InputState model)
  -> InputSpec model err t
  -> supermodel
  -> Reading supermodel err t
readAt lens (InputSpec spec) =
  (_ ^. lens <<< _Newtype <<< prop (Proxy :: Proxy "model"))
  >>> spec.read
  >>> R.fromEither (lens <<< _Newtype <<< prop (Proxy :: Proxy "perturbed"))


-- | Render an input
render :: forall m model err t. MonadEffect m => InputSpec model err t -> InputState model -> Html m (InputState model)
render (InputSpec spec) (InputState { model, perturbed }) =

  E.span
  [ P.onFocusout \_ step -> step (_Newtype <<< prop (Proxy :: Proxy "perturbed") .~ true)
  , P.onInput \_ step -> step (_Newtype <<< prop (Proxy :: Proxy "perturbed") .~ true)
  , P.style'
    [ S.display "contents"
    ]
  ]
  [ spec.render mErr model
      # E.enroot (_Newtype <<< prop (Proxy :: Proxy "model"))
  ]

  # E.hoist liftEffect

  where

  mErr = case perturbed, spec.read model of
    true, Left err -> Just err
    _, _ -> Nothing




-- | Change the model type of an `InputSpec`
-- |
-- | Since an `InputSpec` specifies both how to read *and* how to write an input, changing
-- | its model requires an `Iso'`.
changeModel :: forall model model' err t. Iso' model model' -> InputSpec model err t -> InputSpec model' err t
changeModel iso (InputSpec spec) = InputSpec
  { write: \t -> spec.write t ^. iso
  , read: \model' -> spec.read (model' ^. re iso)
  , empty: spec.empty ^. iso
  , render: \mErr model' -> spec.render mErr (model' ^. re iso) # E.enroot (re iso)
  }


-- | Change the output type of an `InputSpec`
-- |
-- | Since an `InputSpec` specifies both how to read *and* how to write an input, changing
-- | its output type requires an `Iso'`.
changeOutput :: forall model err t t'. Iso' t t' -> InputSpec model err t -> InputSpec model err t'
changeOutput iso (InputSpec spec) =
  InputSpec $ spec
    { write = \t -> spec.write (t ^. re iso)
    , read = \model -> spec.read model # rmap (_ ^. iso)
    }


-- | Add a condition to an input
withValidation :: forall model err t. Semigroup err => (model -> Either err Unit) -> InputSpec model err t -> InputSpec model err t
withValidation cond (InputSpec spec) =
  InputSpec $ spec
    { read = \model ->
        case spec.read model, cond model of
          Left err, Left err' -> Left (err <> err')
          Left err, Right _ -> Left err
          Right _, Left err -> Left err
          Right val, Right _ -> Right val
    }


-- | Post-process an input
-- |
-- | Prefer `withValidation` when possible, as it collates errors better.
-- |
-- | Specifically, if an inputted value is invalid and fails a `withValidation` check, then both errors
-- | will be collected. However, an invalid inputted value will never even be passed into `withPostprocess`
-- | and hence any errors from `withPostprocess` will not be collected.
withPostprocess :: forall model err t. (t -> Either err t) -> InputSpec model err t -> InputSpec model err t
withPostprocess post (InputSpec spec) =
  InputSpec $ spec
    { read = spec.read >=> post
    }


-- | Make an input optional by choosing a default value
-- |
-- | If the input's empty state is not invalid, then this operation has no effect
makeOptional :: forall model err t. Eq model => t -> InputSpec model err t -> InputSpec model err t
makeOptional def't (InputSpec spec) =
  InputSpec $ spec
    { read = \model ->
        case spec.read model of
          Right v -> Right v
          Left err ->  -- apply default only on read failure
            if model == spec.empty
            then Right def't
            else Left err
    }


-- | Add an `err` display to an input
-- |
-- | Try `stdWithErrors` for the first argument
withErrorDisplay :: forall model err t. (forall a m. err -> Html m a -> Html m a) -> InputSpec model err t -> InputSpec model err t
withErrorDisplay renderWithErrors (InputSpec spec) =
  InputSpec $ spec
    { render = \mErr model ->
        spec.render mErr model
        # case mErr of
            Nothing -> identity
            Just err -> renderWithErrors err
    }


-- | Standard way to display errors
stdWithErrors :: forall m a. Array String -> Html m a -> Html m a
stdWithErrors errs html =
  fold
  [ html
  , case errs of
      [] -> mempty
      _ ->
        E.ul
        []
        [ errs # foldMap \err ->
            E.li
            [ P.style'
              [ S.color "red"
              ]
            ]
            [ E.text err
            ]
       ]
  ]


-- | Take a universal-`err` spec and monomorphize to `err ~ Void`
totalize :: forall model t. (forall err. InputSpec model err t) -> InputSpec model Void t
totalize spec = spec

-- | Discharge `err ~ Void` back into a universal
partialize :: forall model err t. InputSpec model Void t -> InputSpec model err t
partialize (InputSpec spec) =
  InputSpec $ spec
    { read = spec.read >>> bimap absurd identity
    , render = \_mErr model -> spec.render Nothing model
    }


--------------------------------------------------------------------------------

-- nb. Each input model type gets is own newtype. The reason for this is so that
-- use-sites store in their model eg. 'InputState PasswordInputModel' rather than
-- the much-less descriptive 'InputState String'. Each model newtype also
-- derives a Newtype instance. The reason for this is to expose the actual model
-- type to the user so that they may use 'withCondition'. This means that the
-- underlying model type of an input is part of the input's public API!


-- | - `props` -- properties to place on the `<input>`
type StdInputOpts = Opts
  { props :: forall m s. Array (Prop m s)
  }


newtype StringInputModel = StringInputModel String

derive instance Newtype StringInputModel _
derive newtype instance UnsureEq StringInputModel

stringInput :: forall err. StdInputOpts -> InputSpec StringInputModel err String
stringInput fromDefaults =
  mkUnfailableInputSpec
    { write: identity
    , read: Right
    , empty: ""
    , render: \s ->
        E.input
        [ P.value s
        , P.onInput' \s' step -> step (const s')
        , fold opts.props
          # P.enroot (re _Newtype :: Iso' String StringInputModel)
        ]
    }
  # changeModel (re _Newtype :: Iso' _ StringInputModel)

  where

  opts = fromDefaults
    { props: []
    }


newtype NonemptyStringInputModel = NonemptyStringInputModel String

derive instance Newtype NonemptyStringInputModel _
derive newtype instance UnsureEq NonemptyStringInputModel

nonemptyStringInput :: StdInputOpts -> InputSpec NonemptyStringInputModel (Array String) String
nonemptyStringInput opts =
  stringInput opts
  # changeModel (_Newtype :: Iso' _ String)
  # withErrorDisplay stdWithErrors
  # withValidation (\val -> if val == "" then Left ["Cannot be empty"] else Right unit)
  # changeModel (re _Newtype :: Iso' String _)


newtype Email = Email String
newtype EmailInputModel = EmailInputModel String
derive newtype instance UnsureEq EmailInputModel

derive instance Newtype Email _
derive instance Newtype EmailInputModel _

-- | Email input
-- |
-- | Validation checks that given email matches regex `.+@.+`; that is, the email
-- | is one or more characters followed by `@` followed by one or more characters.
-- |
-- | This validation is intentionally lenient. Email address syntax is surprisingly
-- | complex, allowing for far more valid emails than one might expect, including
-- | emails with multiple `@` signs, quotation marks, and spaces. It would be
-- | possible to implement this syntax, but not worth it: at the end of the day
-- | to know if a given email address "is valid" requires sending a verification
-- | email to it, which will fail if the syntax is invalid anyway.
emailInput :: StdInputOpts -> InputSpec EmailInputModel (Array String) Email
emailInput opts =
  stringInput opts
  # changeModel (_Newtype :: Iso' _ String)
  # withErrorDisplay stdWithErrors
  # withValidation (\val -> case split1 "@" val of
      Nothing -> Left ["Must contain an @"]
      Just (before /\ after) ->
        let bErrs = if before == "" then ["Must have some text before the @"] else []
            aErrs = if after  == "" then ["Must have some text after the @"] else []
            errs = bErrs <> aErrs
        in if null errs then Right unit else Left errs)
  # changeModel (re _Newtype :: Iso' String _)
  # changeOutput (re _Newtype :: Iso' String _)

  where

  split1 :: String -> String -> Maybe (String /\ String)
  split1 delim str =
    Str.split (Str.Pattern delim) str
    # List.fromFoldable
    # case _ of
      Nil -> Nothing  -- impossible
      Cons all Nil -> Nothing  -- didn't split
      Cons before allAfter -> Just (before /\ intercalate delim allAfter)


newtype Password = Password String
newtype PasswordInputModel = PasswordInputModel String

derive instance Newtype Password _
derive instance Newtype PasswordInputModel _
derive newtype instance UnsureEq PasswordInputModel

-- FIXME: default <input type>s ?

type PasswordOpts = Opts
  { mustMatch :: Maybe String
  , inputOpts :: StdInputOpts
  }

-- | Password input
-- |
-- | Validation checks that:
-- | 1. input is nonempty; and
-- | 2. input value matches the given value
passwordInput :: PasswordOpts -> InputSpec PasswordInputModel (Array String) Password
passwordInput fromDefaults =
  stringInput opts.inputOpts
  # changeModel (_Newtype :: Iso' _ String)
  # withErrorDisplay stdWithErrors
  # withValidation (\val -> if val == "" then Left ["Password be empty"] else Right unit)
  # case opts.mustMatch of
      Nothing -> identity
      Just expected -> withValidation (\val -> if val == expected then Right unit else Left ["Passwords do not match"])
  # changeModel (re _Newtype :: Iso' _ PasswordInputModel)
  # changeOutput (re _Newtype :: Iso' _ Password)

  where
  opts =
    fromDefaults
      { mustMatch: Nothing
      , inputOpts: def
      }

