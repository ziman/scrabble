module Component.Board (new) where

import Prelude
import Matrix (Matrix)
import Matrix as Matrix
import Data.Maybe (Maybe(..))

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Component.Letter as Letter

type Props = Unit
type State =
  { letters :: Matrix (Maybe Letter.Props)
  }

render :: Self Props State -> JSX
render self =
  R.table
  { className: "board"
  , children:
     Matrix.rows self.state.letters <#> \row ->
      R.tr
      { children:
          row <#> \mbProps ->
            R.td
            { children:
                case mbProps of
                  Just props -> [Letter.new props]
                  Nothing -> []
            }
      }
  }

new :: Props -> JSX
new = make (createComponent "Board")
  { initialState:
    { letters: Matrix.repeat 15 15 Nothing
    }
  , render
  }

-- vim: et ts=2 sts=2 sw=2
