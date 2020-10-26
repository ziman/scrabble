module Component.Letters (new) where

import Prelude

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Api as Api
import Component.Letter as Letter

type Props = { letters :: Array Api.Letter }
type State = Unit

render :: Self Props State -> JSX
render self =
  R.ul
  { className: "letters"
  , children: do
      letter <- self.props.letters
      pure $ R.li
        { children: [Letter.new letter]
        }
  }

new :: Props -> JSX
new = make (createComponent "Letters")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
