module Component.VoteButtons (new) where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

type Props =
  { onVote :: Boolean -> Effect Unit
  , vote :: Boolean
  }
type State = Unit

render :: Self Props State -> JSX
render self =
  R.div
  { className: "vote-buttons"
  , children:
    [ R.button
      { children: [R.img {src: "thumbs-up.png"}]
      , onClick: capture_ $ self.props.onVote true
      , disabled: self.props.vote
      }
    , R.button
      { children: [R.img {src: "thumbs-down.png"}]
      , onClick: capture_ $ self.props.onVote false
      , disabled: not self.props.vote
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "VoteButtons")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
