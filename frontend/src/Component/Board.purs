module Component.Board (new) where

import Prelude
import Data.Maybe (Maybe(..))
import Matrix (Matrix)
import Matrix as Matrix

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Api as Api
import Component.Letter as Letter

type Props = Unit
type State =
  { letters :: Matrix Api.Cell
  }

render :: Self Props State -> JSX
render self =
  R.table
  { className: "board"
  , children:
     Matrix.rows self.state.letters <#> \row ->
      R.tr
      { children:
          row <#> \cell ->
            R.td
            { children:
                case cell of
                  Api.Blank Nothing -> []
                  Api.Blank (Just _boost) -> []
                  Api.Played letter -> [Letter.new letter]
            }
      }
  }

new :: Props -> JSX
new = make (createComponent "Board")
  { initialState:
    { letters: Matrix.repeat 15 15 (Api.Blank Nothing)
    }
  , render
  }

-- vim: et ts=2 sts=2 sw=2
