module Component.Letter (new) where

import Prelude
import Data.Maybe (Maybe(..), isJust)

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

import Api as Api
import Utils as Utils

type Props =
  { letter :: Api.Letter
  , spot :: Maybe Api.LetterSpot
  , isUncommitted :: Boolean
  }

type State = Unit

render :: Self Props State -> JSX
render self =
  R.a
  { className: "letter"
    <> if self.props.isUncommitted
         then " uncommitted"
         else " committed"
  , href: "#"
  , draggable: isJust self.props.spot
  , onDragStart:
      case self.props.spot of
        Nothing -> capture_ $ pure unit
        Just spot -> Utils.dragHandler $ pure spot
  , children:
    [ R.span
      { className: "value"
      , children: [R.text $ show self.props.letter.value]
      }
    , R.text self.props.letter.letter
    ]
  }

new :: Props -> JSX
new = make (createComponent "Letter")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
