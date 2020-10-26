module Component.Letter (new) where

import Prelude
import Data.Maybe (Maybe(..))

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, nativeEvent)

import Data.MediaType.Common as MediaType
import Web.HTML.Event.DragEvent as DragEvent
import Web.HTML.Event.DataTransfer as DataTransfer

import Api as Api
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)

type Props =
  { letter :: Api.Letter
  , draggable :: Boolean
  }

type State = Unit

render :: Self Props State -> JSX
render self =
  R.a
  { className: "letter"
  , href: "#"
  , draggable: self.props.draggable
  , onDragStart: capture nativeEvent \evt ->
      case DragEvent.fromEvent evt of
        Nothing -> pure unit
        Just dragEvt ->
          DataTransfer.setData
            MediaType.applicationJSON
            (stringify $ encodeJson self.props.letter)
            (DragEvent.dataTransfer dragEvt)
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
