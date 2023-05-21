module Mation.Examples.Inputs where

import Mation.Core.Prelude

import Mation (Html)
import Mation.Elems as E
import Mation.Experimental.Input as In
import Mation.Experimental.Opts (def)


inputs :: _
inputs =
  { username:
      In.stringInput def
      # In.withValidation  -- Require username nonempty
          (\(In.StringInputModel v) -> if v == "" then Left ["Cannot be empty"] else Right unit)
      # In.withErrorDisplay In.stdWithErrors  -- this line isn't *required*, which is bad ...
  , email: In.emailInput def
  }


type Model =
  { username :: In.InputState In.StringInputModel
  , email :: In.InputState In.EmailInputModel
  }


initial :: Model
initial =
  { username: In.empty inputs.username
  , email: In.empty inputs.email
  }


render :: Model -> Html Effect Model
render model =
  E.form
  []

  [ In.render inputs.username model.username
    # withLabel "Username"
    # E.enroot (prop (Proxy :: Proxy "username"))

  , In.render inputs.email model.email
    # withLabel "Email"
    # E.enroot (prop (Proxy :: Proxy "email"))
  ]

  where

  withLabel :: forall m s. String -> Html m s -> Html m s
  withLabel lbl html =
    fold
    [ E.p []
      [ E.label [] [ E.text lbl ]
      ]
    , html
    ]
