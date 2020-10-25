module Component.Board (new) where

import Prelude
import Data.Array((..))

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Component.Letter as Letter

type Props = Unit
type State = Unit

render :: Self Props State -> JSX
render self =
  R.table
  { className: "board"
  , children: do
     row <- 0..14
     pure $ R.tr
      { children: do
          col <- 0..14
          pure $ R.td
            { children:
              [ Letter.new
                { letter: "X"
                , value: 10
                }
              ]
            }
      }
  }

new :: Props -> JSX
new = make (createComponent "Board")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
