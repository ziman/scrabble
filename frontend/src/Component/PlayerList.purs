module Component.PlayerList (new) where

import Prelude
import Data.Array (replicate)

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Api as Api

type Props = { players :: Array Api.Player }
type State = Unit

render :: Self Props State -> JSX
render self =
  R.table
  { className: "player-list"
  , children:
    [ R.tbody
      { children: do
          player <- self.props.players
          pure $ R.tr
            { className:
                if player.isAlive
                  then "alive"
                  else "dead"
            , children:
              [ R.td
                { className: "player-name"
                , children: [R.text player.name]
                }
              , R.td
                { className: "player-score"
                , children: [R.text $ show player.score]
                }
              , R.td
                { className: "player-letters"
                , children:
                    if not player.isAlive
                      then [R.img {src: "dead.png"}]
                      else if player.vote
                        then [R.img {src: "thumbs-up.png"}]
                        else
                          replicate player.letters $
                            R.span {className: "letter-dot"}
                }
              ]
            }
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "UserList")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
