module Component.PlayerList (new) where

import Prelude

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Api as Api

type Props = { players :: Array Api.Player }
type State = Unit

render :: Self Props State -> JSX
render self =
  R.ul
  { className: "player-list"
  , children: do
      player <- self.props.players
      pure $ R.li
        { children: [R.text player.name]
          <> if player.isAlive
              then []
              else [R.text "☠"]
        }
  }

new :: Props -> JSX
new = make (createComponent "UserList")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
