module Component.VoteButtons (new) where

import Prelude
import Data.Array (length)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

import Api as Api
import Utils as Utils
import Component.Letter as Letter

type Props =
  { onVote :: Maybe (Maybe Boolean -> Effect Unit)
  , vote :: Maybe Boolean
  }
type State = Unit

render :: Self Props State -> JSX
render self =
  R.div
  { className: "vote-buttons"
  , children:
    [ R.button
      { children: [R.img {src: "thumbs-up.png"}]
      }
    , R.button
      { children: [R.img {src: "thumbs-down.png"}]
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "VoteButtons")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
