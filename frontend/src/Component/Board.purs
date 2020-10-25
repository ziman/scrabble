module Component.Board (new) where

import Prelude
import Data.Maybe (Maybe(..))

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Api as Api
import Component.Letter as Letter

type Props = Api.Board
type State = Unit

render :: Self Props State -> JSX
render self =
  R.table
  { className: "board"
  , children:
    [ R.tbody
      { children:
        self.props.cells <#> \row ->
          R.tr
          { children:
              row <#> \cell ->
                R.td
                { children:
                    case cell of
                      Api.Blank Nothing -> []
                      Api.Blank (Just boost) -> [R.text "B"]
                      Api.Played letter -> [Letter.new letter]
                }
          }
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "Board")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
