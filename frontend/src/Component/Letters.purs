module Component.Letters (new) where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

import Api as Api
import Component.Letter as Letter

type Props =
  { letters :: Array Api.Letter
  , onGetLetter :: Effect Unit
  }
type State = Unit

render :: Self Props State -> JSX
render self =
  R.ul
  { className: "letters"
  , children: 
    ( do
      letter <- self.props.letters
      pure $ R.li
        { children: [Letter.new {letter, draggable: true}]
        }
    ) <> [
      R.li
      { children:
        [ R.button
          { children: [R.text "+"]
          , onClick: capture_ self.props.onGetLetter
          }
        ]
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "Letters")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
