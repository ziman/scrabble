module Component.PlayerList (new) where

import Prelude

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

type Props = { players :: Array String }
type State = Unit

render :: Self Props State -> JSX
render self =
  R.ul
  { className: "player-list"
  , children: do
      playerName <- self.props.players
      pure $ R.li
        { children: [R.text playerName]
        }
  }

new :: Props -> JSX
new = make (createComponent "UserList")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
