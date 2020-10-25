module Component.Placeholder (new) where

import Prelude
import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

type Props = {title :: String}
type State = Unit

render :: Self Props State -> JSX
render self =
  R.div
  { className: self.props.title <> " placeholder"
  , children: [R.text self.props.title]
  }

new :: Props -> JSX
new = make (createComponent "Placeholder")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
