module Component.Game (new) where

import Prelude

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Component.Login as Login
import Component.Board as Board
import Component.Placeholder as UserList
import Component.Placeholder as Letters

type Props = Unit
type Cookie = String
data State
  = LoggedOut
  | LoggedIn Cookie

render :: Self Props State -> JSX
render self =
  case self.state of
    LoggedOut ->
      R.div
      { className: "game"
      , children:
        [ Login.new
          { onSubmit: \_playerName -> pure unit
          }
        ]
      }
    LoggedIn _cookie ->
      R.div
      { className: "game"
      , children:
        [ UserList.new {title: "user-list"}
        , R.div
          { className: "main"
          , children:
            [ Board.new unit
            , Letters.new {title: "letters"}
            ]
          }
        ]
      }

new :: Props -> JSX
new = make (createComponent "Game")
  { initialState: LoggedOut
  , render
  }

-- vim: et ts=2 sts=2 sw=2
