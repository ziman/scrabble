module Component.Login(new, Props) where

import Effect (Effect)
import Prelude (Unit, unit, ($))
import Control.Applicative (pure)
import Data.Maybe (Maybe(..))

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, capture_, targetValue)

type Props =
  { onSubmit :: String -> Effect Unit
  }
type State =
  { playerName :: String
  }

render :: Self Props State -> JSX
render self =
  R.form
  { className: "login"
  , children:
    [ R.label
      { children: [R.text "Name you want to use:"]
      , htmlFor: "playerName"
      }
    , R.input
      { type: "text"
      , name: "playerName"
      , id: "playerName"
      , onChange: capture targetValue
          case _ of
            Nothing -> pure unit
            Just playerName ->
              self.setState \s -> s{playerName = playerName}
      }
    , R.input
      { type: "submit"
      , value: "Log in"
      }
    ]
  , onSubmit: capture_ $
      self.props.onSubmit self.state.playerName
  }

new :: Props -> JSX
new = make (createComponent "Login")
  { initialState: {playerName: ""}
  , render
  }

-- vim: et ts=2 sts=2 sw=2
