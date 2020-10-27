module Component.Letter (new) where

import Prelude
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.Events (handler)
import React.Basic.DOM.Events (nativeEvent)

import Data.MediaType.Common as MediaType
import Web.HTML.Event.DragEvent as DragEvent
import Web.HTML.Event.DataTransfer as DataTransfer

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)

import Api as Api

type Props =
  { letter :: Api.Letter
  , spot :: Maybe Api.LetterSpot
  }

type State = Unit

render :: Self Props State -> JSX
render self =
  R.a
  { className: "letter"
  , href: "#"
  , draggable: isJust self.props.spot
  , onDragStart: handler nativeEvent \evt ->
      case Tuple (DragEvent.fromEvent evt) self.props.spot of
        Tuple (Just dragEvt) (Just spot) -> do

          DataTransfer.setData
            MediaType.applicationJSON
            (stringify $ encodeJson spot)
            (DragEvent.dataTransfer dragEvt)

          DataTransfer.setDropEffect
            DataTransfer.Move
            (DragEvent.dataTransfer dragEvt)

        _ -> pure unit
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
