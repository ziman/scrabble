module Component.Board (new) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Array (zip, (..), length)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_, capture, nativeEvent)

import Data.MediaType.Common as MediaType
import Web.HTML.Event.DragEvent as DragEvent
import Web.HTML.Event.DataTransfer as DataTransfer

import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (decodeJson)

import Api as Api
import Utils as Utils
import Component.Letter as Letter

type Props = Api.Board
type State =
  { dropCoords :: Maybe (Tuple Int Int)
  }

render :: Self Props State -> JSX
render self =
  R.table
  { className: "board"
  , children:
    [ R.tbody
      { children:
        enumerate self.props.cells <#> \(Tuple i row) ->
          R.tr
          { children:
              enumerate row <#> \(Tuple j cell) ->
                R.td
                { className:
                  (
                    case self.state.dropCoords of
                      Just ij | ij == Tuple i j -> "drop "
                      _ -> ""
                  ) <> (
                    case cell.boost of
                      Just Api.DoubleLetter -> "double-letter"
                      Just Api.TripleLetter -> "triple-letter"
                      Just Api.DoubleWord -> "double-word"
                      Just Api.TripleWord -> "triple-word"
                      Nothing -> "cell"
                  )

                , onDragOver: capture_ do
                    self.setState \s -> s{ dropCoords = Just (Tuple i j) }

                , onDrop: capture nativeEvent \evt -> do
                    case DragEvent.fromEvent evt of
                      Nothing -> pure unit
                      Just dragEvt -> do
                        doc <- DataTransfer.getData
                          MediaType.applicationJSON
                          (DragEvent.dataTransfer dragEvt)

                        case jsonParser doc of
                          Left err -> Utils.log err
                          Right json -> case decodeJson json of
                            Left err -> Utils.log $ show err
                            Right letter -> Utils.log $ show (letter :: Api.Letter)

                , children:
                    case cell.letter of
                      Nothing -> []
                      Just letter -> [Letter.new {letter, draggable: false}]
                }
          }
      }
    ]
  }

enumerate :: forall a. Array a -> Array (Tuple Int a)
enumerate xs = zip (0 .. (length xs - 1)) xs

new :: Props -> JSX
new = make (createComponent "Board")
  { initialState:
    { dropCoords: Nothing
    }
  , render
  }

-- vim: et ts=2 sts=2 sw=2
