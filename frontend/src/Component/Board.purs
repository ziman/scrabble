module Component.Board (new) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Effect (Effect)

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_, capture, nativeEvent)
import React.Basic.Events (handler_)

import Data.MediaType.Common as MediaType
import Web.HTML.Event.DragEvent as DragEvent
import Web.HTML.Event.DataTransfer as DataTransfer

import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (decodeJson)

import Api as Api
import Utils as Utils
import Component.Letter as Letter

type Props =
  { board :: Api.Board
  , onLetterDrop :: Api.LetterSpot -> Api.LetterSpot -> Effect Unit
  }
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
        Utils.enumerate self.props.board.cells <#> \(Tuple i row) ->
          R.tr
          { children:
              Utils.enumerate row <#> \(Tuple j cell) ->
                R.td
                { className:
                  (
                    case self.state.dropCoords of
                      Just ij | ij == Tuple i j -> "drop "
                      _ -> "nondrop "
                  ) <> (
                    case cell.boost of
                      Just Api.DoubleLetter -> "double-letter"
                      Just Api.TripleLetter -> "triple-letter"
                      Just Api.DoubleWord -> "double-word"
                      Just Api.TripleWord -> "triple-word"
                      Nothing -> "cell"
                  )

                , onDragOver:
                    case cell.letter of
                      Just _ -> handler_ $ pure unit  -- reject drop
                      Nothing -> capture_ $ pure unit  -- accept drop

                , onDragEnter:
                    case cell.letter of
                      Just _ -> handler_ $ pure unit  -- do not preventDefault => reject drop
                      Nothing -> capture_ $
                        self.setState \s -> s{ dropCoords = Just (Tuple i j) }

                , onDrop: capture nativeEvent \evt -> do
                    self.setState \s -> s{ dropCoords = Nothing }
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
                            Right srcSpot -> self.props.onLetterDrop srcSpot (Api.Board i j)

                , children:
                    case cell.letter of
                      Nothing -> []
                      Just letter ->
                        [ Letter.new
                          { letter
                          , spot: Just (Api.Board i j)
                          }
                        ]
                }
          }
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "Board")
  { initialState:
    { dropCoords: Nothing
    }
  , render
  }

-- vim: et ts=2 sts=2 sw=2
