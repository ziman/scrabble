module Component.Game (new) where

import Prelude

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Component.Board as Board
import Component.Placeholder as UserList
import Component.Placeholder as Letters

type Props = Unit
type State = Unit

render :: Self Props State -> JSX
render self =
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
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
