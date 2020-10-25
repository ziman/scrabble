module Component.Letter (new, Props) where

import Prelude

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

type Props =
  { letter :: String
  , value :: Int
  }
type State = Unit

render :: Self Props State -> JSX
render self =
  R.span
  { className: "letter"
  , children:
    [ R.span
      { className: "value"
      , children: [R.text $ show self.props.value]
      }
    , R.text self.props.letter
    ]
  }

new :: Props -> JSX
new = make (createComponent "Letter")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
