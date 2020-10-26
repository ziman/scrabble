module Component.Board (new) where

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
import Utils as Utils
import Component.Letter as Letter

type Props = Api.Board
type State = Unit

render :: Self Props State -> JSX
render self =
  R.table
  { className: "board"
  , children:
    [ R.tbody
      { children:
        self.props.cells <#> \row ->
          R.tr
          { children:
              row <#> \cell ->
                R.td
                { className:
                    case cell.boost of
                      Just Api.DoubleLetter -> "double-letter"
                      Just Api.TripleLetter -> "triple-letter"
                      Just Api.DoubleWord -> "double-word"
                      Just Api.TripleWord -> "triple-word"
                      Nothing -> "cell"

                , onDragOver: capture nativeEvent \evt -> do
                    case DragEvent.fromEvent evt of
                      Nothing -> pure unit
                      Just dragEvt -> do
                        Utils.log $ show $ DataTransfer.types (DragEvent.dataTransfer dragEvt)
                        d <- DataTransfer.getData MediaType.textPlain (DragEvent.dataTransfer dragEvt)
                        Utils.log d

                    Utils.log "drag over!"
                    pure unit

                , onDrop: capture nativeEvent \evt -> do
                    Utils.log "drop!"
                    pure unit

                , children:
                    case cell.letter of
                      Nothing -> []
                      Just letter -> [Letter.new {letter, draggable: false}]
                }
          }
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "Board")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
