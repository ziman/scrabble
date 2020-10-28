module Component.Board (new) where

import Prelude
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set

import Effect (Effect)

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

import Api as Api
import Utils as Utils
import Component.Letter as Letter

type Props =
  { board :: Api.Board
  , uncommitted :: Set (Tuple Int Int)
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
                      Just _ -> Utils.rejectDrop
                      Nothing -> Utils.acceptDrop

                , onDragEnter:
                    case cell.letter of
                      Just _ -> Utils.rejectDrop
                      Nothing -> capture_ $
                        self.setState \s -> s{ dropCoords = Just (Tuple i j) }

                , onDrop: Utils.dropHandler \srcSpot -> do
                    self.setState \s -> s{ dropCoords = Nothing }
                    self.props.onLetterDrop srcSpot (Api.Board i j)

                , children:
                    let isUncommitted = Tuple i j `Set.member` self.props.uncommitted in
                    case cell.letter of
                      Nothing -> []
                      Just letter ->
                        [ Letter.new
                          { letter
                          , spot:
                              if isUncommitted
                                then Just (Api.Board i j)
                                else Nothing  -- can't move anymore
                          , isUncommitted
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
